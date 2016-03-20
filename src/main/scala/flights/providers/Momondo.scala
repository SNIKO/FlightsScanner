package flights.providers

import java.time.OffsetDateTime

import api.momondo._
import flights.{Trip, Fare, FaresProvider, FaresProviderError, Itinerary}
import utils.Implicits._
import utils.Utils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.EitherT

case class References(airlines: Seq[Airline],
                      airports: Seq[Airport],
                      flights: Seq[Flight],
                      legs: Seq[Leg],
                      segments: Seq[Segment],
                      offers: Seq[Offer],
                      ticketClasses: Seq[TicketClass]) {

  def update(searchResult: SearchResult): References = {
    References(
      airlines ++ searchResult.airlines,
      airports ++ searchResult.airports,
      flights ++ searchResult.flights,
      legs ++ searchResult.legs,
      segments ++ searchResult.segments,
      offers ++ searchResult.offers,
      ticketClasses ++ searchResult.ticketClasses)
  }
}

object References {
  def empty: References = {
    References(Seq.empty[Airline], Seq.empty[Airport], Seq.empty[Flight], Seq.empty[Leg], Seq.empty[Segment], Seq.empty[Offer], Seq.empty[TicketClass])
  }
}

class Momondo extends FaresProvider {

  override def search(trip: Trip): FutureActionResult[FaresProviderError, Seq[Fare]] = {
    val searchRequest = api.momondo.SearchRequest(
      adultCount = 1,
      culture = "en-US",
      ticketClass = "ECO",
      segments = trip.flights.map(d => Direction(d.date, d.fromAirport, d.toAirport)))

    val res = for {
      session <- EitherT(api.momondo.Client.startSearch(searchRequest))
      fares   <- EitherT(pollResults(session.searchId, session.engineId))
    } yield fares

    res.leftMap(error => FaresProviderError("Momondo", s"Failed to load fares for route '$trip'. $error")).run
  }

  def pollResults(searchId: String, engineId: Int, isDone: Boolean = false, references: References = References.empty): FutureActionResult[String, Seq[Fare]] =
    if (isDone)
      Future.successful(ActionSuccess(Seq.empty[Fare]))
    else {
      val allFares = for {
        pollResult        <- EitherT(api.momondo.Client.pollSearchResult(searchId, engineId))
        updatedReferences =  references.update(pollResult)
        fares             <- EitherT(Future(parse(pollResult.suppliers, updatedReferences)))
        restFares         <- EitherT(pollResults(pollResult.searchId, pollResult.engineId, pollResult.done, updatedReferences))
      } yield fares ++ restFares

      allFares.run
    }

  // TODO: error handling
  def parse(suppliers: Seq[Supplier], references: References): ActionResult[String, Seq[flights.Fare]] = try {
    val fares = for {
      supplier <- suppliers
      offer <- supplier.offerIndexes.map(index => references.offers(index))
    } yield {
        val itineraries = references.flights(offer.flightIndex).segmentIndexes.map(i => references.segments(i)).map(segment => {
          val legs = segment.legIndexes.map(l => references.legs(l)).map(leg => {
            val airline = references.airlines(leg.airlineIndex)
            val origin = references.airports(leg.originIndex)
            val destination = references.airports(leg.destinationIndex)
            val departureDate = leg.departure.atOffset(timeZoneOffsets(origin.iataCode))
            val ticketClass = references.ticketClasses(offer.ticketClassIndex)

            flights.Flight(departureDate, origin.iataCode, destination.iataCode, airline.iataCode, leg.flightNumber.toString, None, "", getTicketClass(ticketClass.code), ticketClass.name)
          })

          Itinerary(legs)
        })

        Fare(itineraries, OffsetDateTime.now, Seq(flights.PriceInfo(BigDecimal(offer.totalPrice), offer.currency, s"momondo\\${supplier.displayName}")))
      }

   ActionSuccess(fares)
  } catch {
    case e: Throwable => ActionFailure(e.toString)
  }

  def getTicketClass(ticketClass: String) = ticketClass match {
    case "ECO" => flights.TicketClass.Economy
    case other =>
      println(s"Unknown ticket class: $other")
      other
  }

  val timeZoneOffsets = flights.ReferenceData.airports.map(a => (a.iataCode, a.zoneOffset)).toMap
}
