package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = (1 << tile.zoom).toDouble
    Location(
      lat = Math.toDegrees(Math.atan(Math.sinh(Math.PI * (1.0 - 2.0 * tile.y.toDouble / n)))),
      lon = tile.x.toDouble / n * 360.0 - 180.0
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val width = 256
    val height = 256
    val baseX = tile.x*256
    val baseY = tile.y*256

    val xy = for {
      y <- 0 until height
      x <- 0 until width
    } yield (x,y)

    val arr: Array[Pixel] = xy.par.map({case (x,y) =>
      val loc = tileLocation(Tile(baseX + x, baseY + y, tile.zoom + 8))
      val temp = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temp)
      Pixel(col.red, col.green, col.blue, 127)
    }).toArray
    Image.apply(width, height, arr)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      n = 1 << zoom
      x <- 0 until n
      y <- 0 until n
    } {
      generateImage(year, Tile(x, y, zoom), data)
    }
  }

}
