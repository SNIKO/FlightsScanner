import com.typesafe.config.{Config, ConfigFactory}
import org.joda.time.LocalDate
import org.joda.time.format.{DateTimeFormat}
import collection.JavaConversions._

class SearchConfig(val fromAirport: String, val toAirport: String, val minDate: LocalDate, val maxDate: LocalDate, val minDuration: Int, val maxDuration: Int)

object AppConfig {
  private val config = ConfigFactory.load

  val baseFolder = config.getString("appSettings.baseFolder")
  val tripConfigs: Seq[SearchConfig] = config.getConfigList("appSettings.tripConfigs").toList.map(toSearchConfig)

  private def parseDate(date: String) = LocalDate.parse(date, DateTimeFormat.forPattern("dd.MM.yyy"))
  private def toSearchConfig(config: Config) = new SearchConfig(
    config.getString("fromAirport"),
    config.getString("toAirport"),
    parseDate(config.getString("minDate")),
    parseDate(config.getString("maxDate")),
    config.getInt("minDuration"),
    config.getInt("maxDuration"))
}
