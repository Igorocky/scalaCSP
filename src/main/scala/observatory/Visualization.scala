package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EARTH_R_m = 6371000

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val (Some((minDist, minDistTemp)), sumWi, sumWiProd) = temperatures.foldLeft((None:Option[(Double, Temperature)], 0:Double, 0:Double)){
      case ((closestTemp, sumWi, sumWiProd), (loc, temp)) => {
        val dst = dist(loc, location)
        val newClosestTemp = closestTemp match {
          case None => Some((dst, temp))
          case oldClosestTemp@Some((minDist, minDistTemp)) => if (minDist < dst) oldClosestTemp else Some((dst, temp))
        }
        val wiVal = wi(location, loc)
        (newClosestTemp, sumWi + wiVal, sumWiProd + wiVal*temp)
      }
    }
    if (minDist < 1000) {
      minDistTemp
    } else {
      sumWiProd/sumWi
    }
  }

  protected[observatory] def wi(l: Location, li: Location) = pow(dist(l,li), -6)

  protected[observatory] def dist(loc1: Location, loc2: Location): Double = {
    val f1 = toRadians(loc1.lat)
    val f2 = toRadians(loc2.lat)
    val l1 = toRadians(loc1.lon)
    val l2 = toRadians(loc2.lon)
    val dl = abs(l1-l2)
    acos(sin(f1)*sin(f2)+cos(f1)*cos(f2)*cos(dl))*EARTH_R_m
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val (left,right) = points.partition(_._1 < value)
    if (left.isEmpty) {
      points.minBy(_._1)._2
    } else if (right.isEmpty) {
      points.maxBy(_._1)._2
    } else {
      val min = left.maxBy(_._1)
      val max = right.minBy(_._1)
      Color(
        lineInterp(min._1, min._2.red, max._1, max._2.red, value),
        lineInterp(min._1, min._2.green, max._1, max._2.green, value),
        lineInterp(min._1, min._2.blue, max._1, max._2.blue, value)
      )
    }
  }

  protected[observatory] def lineInterp(x0: Double, y0: Int, x1: Double, y1: Int, x: Double): Int = {
    val dbl = y0 + (x-x0)*(y1-y0)/(x1-x0)
    val res = Math.round(dbl).toInt
    res
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180
    val arr: Array[Pixel] = (for {
      y <- 0 until height
      x <- 0 until width
    } yield {
      val loc = Location(90 - y, x - 180)
      val temp = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temp)
      Pixel(col.red, col.green, col.blue, 1)
    }).toArray
    Image.apply(width, height, arr)
  }

}

