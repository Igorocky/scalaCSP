package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {
  private val F2C_CONST = 5.0/9.0;

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationToLocation = Source.fromInputStream(this.getClass.getResourceAsStream(stationsFile)).getLines().toStream
      .map(_.split(','))
      .filter(_.size >= 4)
      .filter(line => line(2) != "" && line(3) != "")
      .map(line => (StationId(line(0), line(1)), Location(line(2).toDouble, line(3).toDouble)))
      .toMap

    Source.fromInputStream(this.getClass.getResourceAsStream(temperaturesFile)).getLines().toStream
      .map(_.split(','))
      .filter(_.size >= 5)
      .filter(line => line(2) != "" && line(3) != "" && line(4) != "")
      .map(line => (
        LocalDate.of(year, line(2).toInt, line(3).toInt),
        StationId(line(0), line(1)),
        (line(4).toDouble - 32.0)*F2C_CONST
      ))
      .filter(t => stationToLocation.contains(t._2))
      .map(t => (t._1, stationToLocation(t._2), t._3))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.toStream.groupBy(_._2).map{
      case (location, str) => (location, str.map(_._3).sum/str.size)
    }
  }

}
