package config

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.typesafe.config.{Config, ConfigFactory}
import collection.JavaConversions._

class TripConfig(val fromAirports: Seq[String], val toAirports: Seq[String], val minDate: LocalDate, val maxDate: LocalDate, val minDuration: Int, val maxDuration: Int)

object AppConfig {

  private val config = ConfigFactory.load
  val baseFolder = config.getString("appSettings.baseFolder")
  val tripConfigs = config.getConfigList("appSettings.tripConfigs").map(toTripConfig).toSeq

  private def parseDate(date: String) = LocalDate.parse(date, DateTimeFormatter.ofPattern("dd.MM.yyy"))

  private def toTripConfig(config: Config) = new TripConfig(
    config.getStringList("fromAirport"),
    config.getStringList("toAirport"),
    parseDate(config.getString("minDate")),
    parseDate(config.getString("maxDate")),
    config.getInt("minDuration"),
    config.getInt("maxDuration"))
}
