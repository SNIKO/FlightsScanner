package api.momondo

import java.time.{LocalDateTime, LocalDate}

case class FlightSearchRequest(adultCount: Int,
                               application: String = "mobileapp",
                               childAges: Seq[Int] = Seq.empty[Int],
                               consumer: String = "cfmetaandroid",
                               culture: String,
                               mobile: Boolean = true,
                               ticketClass: String,
                               segments: Seq[Direction]){
}

case class Direction(departure: LocalDate, origin: String, destination: String)

case class FlightSearchResponse(adultCount: Int,
                                childAges: Seq[Int] = Seq.empty[Int],
                                childCount: Int,
                                culture: String,
                                engineId: Int,
                                infantCount: Int,
                                searchId: String)

case class SearchResult(airlines: Seq[Airline],
                        airports: Seq[Airport],
                        done: Boolean,
                        engineId: Int,
                        error: Boolean,
                        errorMessage: Option[String],
                        fees: Seq[Fee],
                        flights: Seq[Flight],
                        legs: Seq[Leg],
                        offers: Seq[Offer],
                        resultNumber: Int,
                        searchId: String,
                        segments: Seq[Segment],
                        ticketClasses: Seq[TicketClass])

case class Airline(iataCode: String, icaoCode: Option[String], name: String)

case class Airport(countryCode: String,
                   countryName: String,
                   iataCode: String,
                   icaoCode: String,
                   mainCityCode: String,
                   mainCityName: String,
                   name: String,
                   timeZone: Int)

case class Fee(airlineIndex: Option[Int],
               description: String,
               maxAmountEUR: Float,
               minAmountEUR: Float,
               paymentId: Int,
               paymentType: String)

case class Flight(directAirlineIndex: Int, key: String, segmentIndexes: Seq[Int], ticketClassIndex: Int)

case class Leg(airlineIndex: Int,
               flightNumber: Int,
               arrival: LocalDateTime,
               departure: LocalDateTime,
               duration: Int,
               originIndex: Int,
               destinationIndex: Int,
               key: String,
               stopOverCodeIndexes: Seq[Int],
               stopOvers: Int)

case class Offer(adultPrice: Float,
                 adultPriceEUR: Float,
                 adultPriceExclTax: Float,
                 currency: String,
                 feeIndexes: Seq[Int],
                 flightIndex: Int,
                 ticketClassIndex: Int,
                 totalIsCalculated: Boolean,
                 totalPrice: Float,
                 totalPriceEUR: Float,
                 totalPriceExclTax: Float)

case class Segment(duration: Int, key: String, legIndexes: Seq[Int])

case class TicketClass(code: String, name: String)