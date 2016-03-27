package flights

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, OffsetDateTime, Duration}

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

    itineraries.zipWithIndex foreach { case (itinerary, index) =>
      sb.appendLine(s"\tFlight $index")
      itinerary.flights.zipWithIndex foreach { case (flight, flightIndex) =>
        val city    = Fare.cities.getOrElse(flight.toAirport, flight.toAirport)
        val airline = Fare.airlines.getOrElse(flight.airline, "Unknown")

        if (flightIndex < itinerary.flights.length - 1) {
          val nextFlight = itinerary.flights(flightIndex + 1)
          val stopover = Duration.between(flight.arrivalDate, nextFlight.departureDate)

          sb.appendLine(f"\t\t${flight.airline} ${flight.flightNumber}%4s $city%10s ${flight.departureDate} $airline, stopover ${stopover.toHours} hours")
        }
        else
          sb.appendLine(f"\t\t${flight.airline} ${flight.flightNumber}%4s $city%10s ${flight.departureDate} $airline")
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
                  arrivalDate      : OffsetDateTime,
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