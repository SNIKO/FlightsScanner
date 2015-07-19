package OneTwoTrip

import com.github.nscala_time.time.Imports._

case class Plane(code: String, name: String)

case class Segment(departureDate    : DateTime,
                   fromAirport      : String,
                   toAirport        : String,
                   airline          : String,
                   flightNumber     : String,
                   operatedBy       : Option[String],
                   plane            : String,
                   reservationClass : String,
                   cabinClass       : String) {

  override def equals(o: Any) = o match {
    case that: Segment => that.departureDate == departureDate && that.airline == airline && that.flightNumber == flightNumber
    case _ => false
  }

  override def hashCode = departureDate.hashCode + airline.hashCode + flightNumber.hashCode
}

case class Flight(segments: Seq[Segment])

case class Fare(flights: Seq[Flight], price: BigDecimal, date: DateTime) {

  def prettyPrint: String = {
    val route = flights.map(_.segments.head.fromAirport).mkString("-")
    val dates = flights.map(_.segments.head.departureDate.toString("dd MMM")).mkString(", ")

    val sb = new StringBuilder
    sb.append(s"$route \t $dates \t Price: USD ${price.toInt}").append("\n")
    flights.zipWithIndex foreach { case (flight, index) =>
      sb.append(s"\tFlight $index").append("\n")
      flight.segments foreach { case segment =>
        val city    = Fare.cities.getOrElse(segment.toAirport, segment.toAirport)
        val airline = Fare.airlines.getOrElse(segment.airline, "Unknown")

        sb.append(f"\t\t${segment.airline} ${segment.flightNumber}%4s $city%10s ${segment.departureDate} $airline").append("\n")
      }
    }

    sb.mkString
  }
}

object Fare {
  val cities = flights.ReferenceData.airports.map(a => (a.iataCode, a.city)).toMap
  val airlines = flights.ReferenceData.airlines.map(a => (a.iataCode, a.name)).toMap
}