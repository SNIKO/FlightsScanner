package flights.providers

import java.time.OffsetDateTime

import api.momondo._

import flights.{FaresProvider, FlightDirection, FaresProviderError, Fare, Itinerary}

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

case class References(airlines: Seq[Airline],
                      airports: Seq[Airport],
                      flights: Seq[Flight],
                      legs: Seq[Leg],
                      segments: Seq[Segment],
                      ticketClasses: Seq[TicketClass]) {

  def update(searchResult: SearchResult): References = {
    References(
      airlines ++ searchResult.airlines,
      airports ++ searchResult.airports,
      flights ++ searchResult.flights,
      legs ++ searchResult.legs,
      segments ++ searchResult.segments,
      ticketClasses ++ searchResult.ticketClasses)
  }
}

object References {
  def empty: References = {
    References(Seq.empty[Airline], Seq.empty[Airport], Seq.empty[Flight], Seq.empty[Leg], Seq.empty[Segment], Seq.empty[TicketClass])
  }
}

class Momondo extends FaresProvider {
  override def search(directions: Seq[FlightDirection]): Future[Either[FaresProviderError, Seq[Fare]]] = {
    val searchRequest = api.momondo.FlightSearchRequest(
      adultCount = 1,
      culture = "en-US",
      ticketClass = "ECO",
      segments = directions.map(d => Direction(d.date, d.fromAirport, d.toAirport)))

    for {
      maybeSession <- api.momondo.Api.flightSearch(searchRequest)
      fares <- maybeSession match {
        case Right(session) => pollResults(session.searchId, session.engineId)
        case Left(error) => Future.successful(Left(FaresProviderError(error)))
      }
    } yield fares
  }

  def pollResults(searchId: String, engineId: Int, attempts: Int = 3, references: References = References.empty): Future[Either[FaresProviderError, Seq[Fare]]] = {
    for {
      maybeResult <- api.momondo.Api.pollSearchResult(searchId, engineId)
      allFares <- maybeResult match {
        case Right(result) =>
          val updatedReferences = references.update(result)
          val fares = parse(result.offers, updatedReferences)

          if (result.done)
            Future.successful(Right(fares))
          else for {
            restFares <- pollResults(searchId, engineId, 3, updatedReferences)
          } yield restFares.right.map(rest => fares ++ rest)

        case Left(error) =>
          attempts match {
            case left if left > 0 => pollResults(searchId, engineId, left - 1, references)
            case 0 => Future.successful(Left(FaresProviderError(error)))
          }
      }
    } yield allFares
  }

  // TODO: error handling
  def parse(offers: Seq[Offer], references: References): Seq[Fare] = {
    val fares = offers.map(offer => {
      val itineraries = references.flights(offer.flightIndex).segmentIndexes.map(i => references.segments(i)).map(segment => {
        val legs = segment.legIndexes.map(l => references.legs(l)).map(leg => {
          val airline = references.airlines(leg.airlineIndex)
          val origin = references.airports(leg.originIndex)
          val destination = references.airports(leg.destinationIndex)
          val departureDate = leg.departure.atOffset(timeZoneOffsets(origin.iataCode))
          val ticketClass = references.ticketClasses(offer.ticketClassIndex)

          flights.Flight(departureDate, origin.iataCode, destination.iataCode, airline.iataCode, leg.flightNumber.toString, None, "", getTicketClass(ticketClass.code), "")
        })

        Itinerary(legs)
      })

      Fare(itineraries, BigDecimal(offer.totalPrice), offer.currency, OffsetDateTime.now)
    })

    fares
  }

  def getTicketClass(ticketClass: String) = ticketClass match {
    case "ECO" => flights.TicketClass.Economy
    case other =>
      println(s"Unknown ticket class: $other")
      other
  }

  val timeZoneOffsets = flights.ReferenceData.airports.map(a => (a.iataCode, a.zoneOffset)).toMap
}