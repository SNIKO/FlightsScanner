package OneTwoTrip

import com.github.nscala_time.time.Imports._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

class Flight(val from: String, val to: String, val date: LocalDate) {
  override def toString() = date.toString("MMM dd") + " " + from + " -> " + to
}

object Api {
  def search(trip: Seq[Flight]): Future[Array[Char]] = Future { Source.fromURL(getUrl(trip)).toArray }

  private[this] def getRoute(trip: Seq[Flight]): String = trip.map(flight => s"${flight.date.toString("ddMM")}" + flight.from + flight.to).mkString
  private[this] def getUrl(route: Seq[Flight]): String = s"https://secure.onetwotrip.com/_api/searching/startSync/?route=${getRoute(route)}&ad=1&cs=E"
}