package flights

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, OffsetDateTime}

import flights.providers.{Momondo, OneTwoTrip}
import utils.Implicits._
import utils.Utils.FutureActionResult

trait FaresProvider {
  def search(trip: Trip): FutureActionResult[FaresProviderError, Seq[Fare]]
}

object FaresProvider {
  val Momondo = new Momondo()
  val OneTwoTrip = new OneTwoTrip()
}

case class FaresProviderError(provider: String, msg: String)

case class Trip(flights: Seq[FlightDirection]) {
  override def toString = flights.mkString(", ")
}

case class FlightDirection(fromAirport: String, toAirport: String, date: LocalDate) {
  override def toString = fromAirport + "->" + toAirport + " " + DateTimeFormatter.ofPattern("dd MMM").format(date)
}

case class Fare(itineraries: Seq[Itinerary], date: OffsetDateTime, prices: Seq[PriceInfo]) {
  def prettyPrint: String = {
    val route = itineraries.map(_.flights.head.fromAirport).mkString("-")
    val dates = itineraries.map(f => DateTimeFormatter.ofPattern("dd MMM").format(f.flights.head.departureDate)).mkString(", ")

    val sb = new StringBuilder
    sb.appendLine(s"$route \t $dates \t Price: ${prices.sortBy(_.price).mkString(", ")}")

    itineraries.zipWithIndex foreach { case (flight, index) =>
      sb.appendLine(s"\tFlight $index")
      flight.flights foreach { case segment =>
        val city    = Fare.cities.getOrElse(segment.toAirport, segment.toAirport)
        val airline = Fare.airlines.getOrElse(segment.airline, "Unknown")

        sb.appendLine(f"\t\t${segment.airline} ${segment.flightNumber}%4s $city%10s ${segment.departureDate} $airline")
      }
    }

    sb.mkString
  }
}

object Fare {
  val cities   = flights.ReferenceData.airports.map(a => (a.iataCode, a.city)).toMap
  val airlines = flights.ReferenceData.airlines.map(a => (a.iataCode, a.name)).toMap
}

case class Itinerary(flights: Seq[Flight])

case class Flight(departureDate    : OffsetDateTime,
                  fromAirport      : String,
                  toAirport        : String,
                  airline          : String,
                  flightNumber     : String,
                  operatedBy       : Option[String],
                  plane            : String,
                  reservationClass : String,
                  cabinClass       : String) {

  override def equals(o: Any) = o match {
    case that: Flight => that.departureDate == departureDate && that.airline == airline && that.flightNumber == flightNumber
    case _ => false
  }

  override def hashCode = departureDate.hashCode + airline.hashCode + flightNumber.hashCode
}

case class PriceInfo(price: BigDecimal, currency: String, provider: String) {
  override def toString = s"$currency ${price.toInt} ($provider)"
}

object TicketClass {
  val Economy: String = "E"
}