package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val dt = d00 + (d10-d00)*point.x
    val db = d01 + (d11-d01)*point.x
    dt + (db-dt)*point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val width = 256
    val height = 256
    val baseX = tile.x*256
    val baseY = tile.y*256

    val xy = for {
      y <- 0 until height
      x <- 0 until width
    } yield (x,y)

    val arr: Array[Pixel] = xy.par.map{case (x,y) => {
      val loc = tileLocation(Tile(baseX + x, baseY + y, tile.zoom + 8))
      val lonInt = loc.lon.toInt
      val latInt = loc.lat.toInt
      val dLon = loc.lon - lonInt
      val dLat = loc.lat - latInt
      val temp = bilinearInterpolation(
        point = CellPoint(dLon, dLat),
        d00 = grid(GridLocation(latInt, lonInt)),
        d01 = grid(GridLocation(latInt + 1, lonInt)),
        d10 = grid(GridLocation(latInt, lonInt + 1)),
        d11 = grid(GridLocation(latInt + 1, lonInt + 1))
      )
      val col = interpolateColor(colors, temp)
      Pixel(col.red, col.green, col.blue, 127)
    }}.toArray
    Image.apply(width, height, arr)
  }

}
