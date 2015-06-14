package OneTwoTrip

import com.github.nscala_time.time.Imports._
import dispatch._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import utils.Log

class Flight(val from: String, val to: String, val date: LocalDate) {
  override def toString() = date.toString("MMM dd") + " " + from + " -> " + to
}

object Api {

  def search(trip: Seq[Flight]): Future[Either[Throwable, String]] = {
    val u = url(getSearchUrl(trip))

    Http(u).either.map {
      case Left(error) => {
        logError(trip, error.getMessage)
        Left(error)
      }
      case Right(response) => {
        response.getResponseBody match {
          case Errors.requestLimitReached => {
            logError(trip, "Request limit reached")
            Left(new Exception("REQUEST_LIMIT_REACHED"))
          }
          case content => Right(content)
        }
      }
    }
  }

  private[this] object Errors {
    val requestLimitReached = "{\"error\":\"REQUEST_LIMIT_REACHED\"}"
  }

  private[this] def getSearchUrl(trip: Seq[Flight]): String = {
    val route = trip.map(flight => s"${flight.date.toString("ddMM")}" + flight.from + flight.to).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$route&ad=1&cs=E"
  }

  private[this] def logError(trip: Seq[Flight], error: String) = Log(s"An error occurred when loading '$trip': $error")
}