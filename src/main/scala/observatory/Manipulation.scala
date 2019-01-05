package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {
  val width = 360
  val halfWidth = width/2
  val height = 180
  val halfHeight = height/2

  def initPreCalcArray: Array[Array[Temperature]] = {
    val preCalc = new Array[Array[Temperature]](width)
    for (i <- 0 until width) {
      preCalc(i) = new Array[Temperature](height)
    }
    preCalc
  }

  def createGridToTempFunction(preCalc: Array[Array[Temperature]]): GridLocation => Temperature =
    gridLoc => preCalc(gridLoc.lon + halfWidth)(gridLoc.lat + halfHeight - 1)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val preCalc = initPreCalcArray
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      preCalc(x)(y) = Visualization.predictTemperature(temperatures, Location(y - halfHeight + 1, x - halfWidth))
    }
    createGridToTempFunction(preCalc)
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val (sums, cnt) = temperaturess.map(makeGrid).foldLeft((initPreCalcArray, 0)){
      case ((arr, cnt), func) =>
        val sums = initPreCalcArray
        for {
          x <- 0 until width
          y <- 0 until height
        } {
          sums(x)(y) = arr(x)(y) + func(GridLocation(y - halfHeight + 1, x - halfWidth))
        }
        (sums, cnt+1)
    }
    for {
      x <- 0 until width
      y <- 0 until height
    } {
      sums(x)(y) /= cnt
    }
    createGridToTempFunction(sums)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val gridFunc = makeGrid(temperatures)
    gridLoc => gridFunc(gridLoc) - normals(gridLoc)
  }


}

