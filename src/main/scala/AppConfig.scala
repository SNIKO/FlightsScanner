package config

import com.typesafe.config.{Config, ConfigFactory}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import collection.JavaConversions._

class TripConfig(val fromAirports: Seq[String], val toAirports: Seq[String], val minDate: LocalDate, val maxDate: LocalDate, val minDuration: Int, val maxDuration: Int)

object AppConfig {

  private val config = ConfigFactory.load
  val baseFolder = config.getString("appSettings.baseFolder")
  val tripConfigs = config.getConfigList("appSettings.tripConfigs").map(toTripConfig).toSeq

  private def parseDate(date: String) = LocalDate.parse(date, DateTimeFormat.forPattern("dd.MM.yyy"))

  private def toTripConfig(config: Config) = new TripConfig(
    config.getStringList("fromAirport"),
    config.getStringList("toAirport"),
    parseDate(config.getString("minDate")),
    parseDate(config.getString("maxDate")),
    config.getInt("minDuration"),
    config.getInt("maxDuration"))
}
