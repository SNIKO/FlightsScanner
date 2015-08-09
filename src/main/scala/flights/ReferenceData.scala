package flights

import java.time.ZoneOffset

import scala.io.Source

case class Airport(name: String, city: String, country: String, iataCode: String, icaoCode: String, zoneOffset: ZoneOffset, timezone: String)

case class Airline(name: String, country: String, iataCode: String, icaoCode: String)

object ReferenceData {

  private val airportsStream = getClass.getResourceAsStream("/airports.dat")
  private val airlinesStream = getClass.getResourceAsStream("/airlines.dat")
  private val airportsCSV = Source.fromInputStream(airportsStream, "UTF-8").getLines()
  private val airlinesCSV = Source.fromInputStream(airlinesStream, "UTF-8").getLines()

  val airports = airportsCSV.map(line => line.split(';') match {
    case Array(_, name, city, country, iataCode, icaoCode, _, _, _, offset, _, timezone) =>
      val offsetInHours = offset.toDouble
      val hours = offsetInHours.toInt
      val minutes = ((offsetInHours - hours) * 60).toInt

      Airport(name, city, country, iataCode, icaoCode, ZoneOffset.ofHoursMinutes(hours, minutes), timezone)
  }).toSeq

  val airlines = airlinesCSV.map(line => line.split(',') match {
    case Array(_, name, _, iataCode, icaoCode, _, country, _) =>
      Airline(name, country, iataCode, icaoCode)
  }).toSeq
}
