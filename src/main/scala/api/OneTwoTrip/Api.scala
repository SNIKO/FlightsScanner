package api.OneTwoTrip

import java.io.IOException
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

import config.AppConfig
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Try, Failure, Success}

class LimitReachedException extends Exception

object Api {

  // TODO: Rewrite it using Spray
  def search(trip: Seq[Flight]): Future[Try[SearchResponse]] = Future {
    try {
      val searchUrl = getSearchUrl(trip)
      val content = Source.fromURL(searchUrl).mkString

      content match {
        case Errors.requestLimitReached => Failure(new LimitReachedException())
        case faresAsJson => JsonProtocol.parse(faresAsJson) match {
          case Success(fares) =>
            val filePath = AppConfig.baseFolder + getFileName(trip) + ".json"
            Utils.saveToFile(filePath, content)
            Success(fares)
          case Failure(ex) =>
            val filePath = AppConfig.baseFolder + "Errors\\" + getFileName(trip) + ".txt"
            val msg = searchUrl + "\n\n" + ex.getMessage

            Utils.saveToFile(filePath, msg)
            Failure(ex)
        }
      }
    } catch {
      case e: IOException => Failure(e)
    }
  }

  private object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private def getSearchUrl(route: Seq[Flight]): String = {
    val r = route.map(flight => s"${DateTimeFormatter.ofPattern("ddMM").format(flight.date)}" + flight.fromAirport + flight.toAirport).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$r&ad=1&cs=E"
  }

  private def getFileName(flights: Seq[Flight]): String = {
    val route = flights.map(f => DateTimeFormatter.ofPattern("ddMM").format(f.date) + f.fromAirport + f.toAirport).mkString
    val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmssZ").format(OffsetDateTime.now)

    timestamp + " " + route
  }
}