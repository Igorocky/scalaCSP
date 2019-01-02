package observatory

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  test("StationId equals is correct") {
    assert(StationId("010013", "03707") === StationId("010013", "03707"))

    assert(StationId(null, "03707") === StationId(null, "03707"))
    assert(StationId("010013", null) === StationId("010013", null))
    assert(StationId(null, null) === StationId(null, null))

    assert(StationId("x", "03707") !== StationId("010013", "03707"))
    assert(StationId("010013", null) !== StationId("010013", "03707"))
  }
  
}