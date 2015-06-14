package OneTwoTrip

import com.github.nscala_time.time.Imports._
import dispatch._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Flight(val from: String, val to: String, val date: LocalDate) {
  override def toString() = date.toString("MMM dd") + " " + from + " -> " + to
}

object Api {

  def search(trip: Seq[Flight]): Future[Either[Throwable, String]] = {
    val u = url(getSearchUrl(trip))

    Http(u).either.map {
      case Left(error) => {
        println(DateTime.now.toString + ": " + error)
        Left(error)
      }
      case Right(response) => {
        response.getResponseBody match {
          case "{\"error\":\"REQUEST_LIMIT_REACHED\"}" => {
            println(DateTime.now.toString + ": Request limit reached")
            Left(new Exception("REQUEST_LIMIT_REACHED"))
          }
          case content => Right(content)
        }
      }
    }
  }

  private[this] def getSearchUrl(trip: Seq[Flight]): String = {
    val route = trip.map(flight => s"${flight.date.toString("ddMM")}" + flight.from + flight.to).mkString
    s"https://secure.onetwotrip.com/_api/searching/startSync/?route=$route&ad=1&cs=E"
  }
}