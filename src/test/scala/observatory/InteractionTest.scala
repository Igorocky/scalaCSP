package observatory

import observatory.Interaction.tileLocation
import observatory.Visualization.{interpolateColor, lineInterp}
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait InteractionTest extends FunSuite with Checkers {
  test("custom tile must be consistent across zoom levels") {
    val temperatures: Iterable[(Location, Temperature)] = List(
      (Location(45,-90), 10),
      (Location(45,90), 20),
      (Location(-45,-90), 30),
      (Location(-45,90), 40)
    )
    val colors: Iterable[(Temperature, Color)] = List(
      (10, Color(10,10,10)),
      (20, Color(50,50,50)),
      (30, Color(100,100,100)),
      (40, Color(150,150,150))
    )

    val img1 = Interaction.tile(temperatures, colors, Tile(2,1,2))
    val img2 = Interaction.tile(temperatures, colors, Tile(4,2,3))

//    assert(img1.color(128,128) === img2.color(0,0))

    println("img1.color(0,0) = " + img1.color(0,0))
    println("img2.color(0,0) = " + img2.color(0,0))
  }

  test("test tileLocation") {
    assert(tileLocation(Tile(0,0,0)) === Location(85.05112877980659,-180.0))

    assert(tileLocation(Tile(0,0,1)) === Location(85.05112877980659,-180.0))
    assert(tileLocation(Tile(1,0,1)) === Location(85.05112877980659,0.0))
    assert(tileLocation(Tile(0,1,1)) === Location(0.0,-180.0))
    assert(tileLocation(Tile(1,1,1)) === Location(0.0,0.0))

    assert(tileLocation(Tile(3,3,2)) === Location(-66.51326044311186,90.0))
  }

  test("test lineInterp") {
    assert(lineInterp(0,0,2,2,1) === 1)
    assert(lineInterp(-1,-1,1,1,0) === 0.0)
  }

  test("test interpolateColor") {
    assert(interpolateColor(List((1,Color(0,0,0))), 1) === Color(0,0,0))
    assert(interpolateColor(List((1,Color(0,0,0)), (2,Color(100,100,100))), 0) === Color(0,0,0))
    assert(interpolateColor(List((1,Color(0,0,0)), (2,Color(100,100,100))), 3) === Color(100,100,100))
    assert(interpolateColor(List((1,Color(0,0,0)), (2,Color(100,100,100))), 1.5) === Color(50,50,50))
  }



  test("distance test") {
    val a = Location(5, 10)
    val b = Location(-5, 10)

    assert(Visualization.dist(a, a) == 0)
    assert(Visualization.dist(a, b) > 0)
  }

  test("temperatures interpolation test") {
    val temps = List[(Location, Temperature)]((Location(5, 5), 3),(Location(-5, 5), 5))
    var location = Location(5, 5)

    var computed = Visualization.predictTemperature(temps, location)
    var expected = 3
    assert(computed == expected)

    location = Location(0, 3)
    computed = Visualization.predictTemperature(temps, location)
    expected = 4
    assert(computed == expected)

    location = Location(4, 4)
    computed = Visualization.predictTemperature(temps, location)
    assert(computed < 4)
  }

  test("color interp one val") {
    val cols = List[(Temperature, Color)]((1, Color(2, 2, 2)))
    val value = 2

    var computed = Visualization.interpolateColor(cols, value)
    val expected = Color(2, 2, 2)
    assert(computed == expected)
  }

  test("color interp two vals") {
    val cols = List[(Temperature, Color)]((1, Color(2, 2, 2)), (-1, Color(4, 4, 4)))

    var value = 1
    var computed = Visualization.interpolateColor(cols, value)
    var expected = Color(2, 2, 2)
    assert(computed == expected)

    value = 0
    computed = Visualization.interpolateColor(cols, value)
    expected = Color(3, 3, 3)
    assert(computed == expected)
  }
}
