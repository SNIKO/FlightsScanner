package api.momondo

import java.time.format.DateTimeFormatter

import argonaut.Argonaut._
import argonaut._

object JsonProtocol {

  import utils.Implicits._

  implicit def FlightSearchRequestEncoder: EncodeJson[FlightSearchRequest] =
    EncodeJson((r: FlightSearchRequest) => Json(
       "AdultCount" := r.adultCount,
      "Application" := r.application,
        "ChildAges" := jArray(Nil),
          "Culture" := r.culture,
           "Mobile" := r.mobile,
      "TicketClass" := r.ticketClass,
         "Segments" := jArray(r.segments.map(s => s.jencode).toList)
  ))

  implicit def SegmentEncoder: EncodeJson[Direction] =
    EncodeJson(s => Json(
        "Departure" := DateTimeFormatter.ISO_LOCAL_DATE.format(s.departure.atStartOfDay()),
           "Origin" := s.origin,
      "Destination" := s.destination
    ))

  implicit def FlightSearchResponseDecoder: DecodeJson[FlightSearchResponse] =
    DecodeJson(j => for {
       adultCount <- (j --\ "AdultCount").read[Int]
        childAges <- (j --\ "ChildAges").readArrayOf[Int]
       childCount <- (j --\ "ChildCount").read[Int]
          culture <- (j --\ "Culture").read[String]
         engineId <- (j --\ "EngineId").read[Int]
      infantCount <- (j --\ "InfantCount").read[Int]
         searchId <- (j --\ "SearchId").read[String]
    } yield FlightSearchResponse(adultCount, childAges, childCount, culture, engineId, infantCount, searchId))

  implicit def SearchResultDecoder: DecodeJson[SearchResult] =
    DecodeJson(j => for {
           airlines <- (j --\ "Airlines").readArrayOf[Airline]
           airports <- (j --\ "Airports").readArrayOf[Airport]
               done <- (j --\ "Done").read[Boolean]
           engineId <- (j --\ "EngineId").read[Int]
              error <- (j --\ "Error").read[Boolean]
       errorMessage <- (j --\ "ErrorMessage").tryRead[String]
               fees <- (j --\ "Fees").readArrayOf[Fee]
            flights <- (j --\ "Flights").readArrayOf[Flight]
               legs <- (j --\ "Legs").readArrayOf[Leg]
             offers <- (j --\ "Offers").readArrayOf[Offer]
       resultNumber <- (j --\ "ResultNumber").read[Int]
           searchId <- (j --\ "SearchId").read[String]
           segments <- (j --\ "Segments").readArrayOf[Segment]
      ticketClasses <- (j --\ "TicketClasses").readArrayOf[TicketClass]
    } yield SearchResult(airlines, airports, done, engineId, error, errorMessage, fees, flights, legs, offers, resultNumber, searchId, segments, ticketClasses))

  implicit def AirlineDecoder: DecodeJson[Airline] =
    DecodeJson(j => for {
      iataCode <- (j --\ "Iata").read[String]
      icaoCode <- (j --\ "Icao").tryRead[String]
          name <- (j --\ "Name").read[String]
    } yield Airline(iataCode, icaoCode, name))

  implicit def AirportDecoder: DecodeJson[Airport] =
    DecodeJson(jAirport => for {
       countryCode <- (jAirport --\ "CountryCode").read[String]
       countryName <- (jAirport --\ "CountryName").read[String]
          iataCode <- (jAirport --\ "Iata").read[String]
          icaoCode <- (jAirport --\ "Icao").read[String]
      mainCityCode <- (jAirport --\ "MainCityCode").read[String]
      mainCityName <- (jAirport --\ "MainCityName").read[String]
              name <- (jAirport --\ "Name").read[String]
          timeZone <- (jAirport --\ "TimeZone").read[Int]
    } yield Airport(countryCode, countryName, iataCode, icaoCode, mainCityCode, mainCityName, name, timeZone))

  implicit def FeeDecoder: DecodeJson[Fee] =
    DecodeJson(j => for {
      airlineIndex <- (j --\ "AirlineIndex").tryRead[Int]
       description <- (j --\ "Description").read[String]
      maxAmountEUR <- (j --\ "MaxAmountEUR").read[Float]
      minAmountEUR <- (j --\ "MinAmountEUR").read[Float]
         paymentId <- (j --\ "PaymentId").read[Int]
       paymentType <- (j --\ "Type").read[String]
    } yield Fee(airlineIndex, description, maxAmountEUR, minAmountEUR, paymentId, paymentType))

  implicit def FlightDecoder: DecodeJson[Flight] =
    DecodeJson(j => for {
          airlineIndex <- (j --\ "DirectAirlineIndex").read[Int]
                   key <- (j --\ "Key").read[String]
        segmentIndexes <- (j --\ "SegmentIndexes").readArrayOf[Int]
      ticketClassIndex <- (j --\ "TicketClassIndex").read[Int]
    } yield Flight(airlineIndex, key, segmentIndexes, ticketClassIndex))

  implicit def LegDecoder: DecodeJson[Leg] =
    DecodeJson(jLeg => for {
          airlineIndex <- (jLeg --\ "AirlineIndex").read[Int]
               arrival <- (jLeg --\ "Arrival").asLocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
             departure <- (jLeg --\ "Departure").asLocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      destinationIndex <- (jLeg --\ "DestinationIndex").read[Int]
           originIndex <- (jLeg --\ "OriginIndex").read[Int]
              duration <- (jLeg --\ "Duration").read[Int]
          flightNumber <- (jLeg --\ "FlightNumber").read[Int]
                   key <- (jLeg --\ "Key").read[String]
       stopOverIndexes <- (jLeg --\ "StopOverCodeIndexes").readArrayOf[Int]
             stopOvers <- (jLeg --\ "StopOvers").read[Int]
    } yield Leg(airlineIndex, flightNumber, arrival, departure, duration, originIndex, destinationIndex, key, stopOverIndexes, stopOvers))

  implicit def OfferDecoder: DecodeJson[Offer] =
    DecodeJson(jOffer => for {
              adultPrice <- (jOffer --\ "AdultPrice").read[Float]
           adultPriceEUR <- (jOffer --\ "AdultPriceEUR").read[Float]
       adultPriceExclTax <- (jOffer --\ "AdultPriceExclTax").read[Float]
                currency <- (jOffer --\ "Currency").read[String]
              feeIndexes <- (jOffer --\ "FeeIndexes").readArrayOf[Int]
             flightIndex <- (jOffer --\ "FlightIndex").read[Int]
        ticketClassIndex <- (jOffer --\ "TicketClassIndex").read[Int]
       totalIsCalculated <- (jOffer --\ "TotalIsCalculated").read[Boolean]
              totalPrice <- (jOffer --\ "TotalPrice").read[Float]
           totalPriceEUR <- (jOffer --\ "TotalPriceEUR").read[Float]
       totalPriceExclTax <- (jOffer --\ "TotalPriceExclTax").read[Float]
    } yield Offer(adultPrice, adultPriceEUR, adultPriceExclTax, currency, feeIndexes, flightIndex, ticketClassIndex, totalIsCalculated, totalPrice, totalPriceEUR, totalPriceExclTax))

  implicit def SegmentDecoder: DecodeJson[Segment] =
    DecodeJson(jSegment => for {
        duration <- (jSegment --\ "Duration").read[Int]
             key <- (jSegment --\ "Key").read[String]
      legIndexes <- (jSegment --\ "LegIndexes").readArrayOf[Int]
    } yield Segment(duration, key, legIndexes))

  implicit def TicketClassDecoder: DecodeJson[TicketClass] =
    DecodeJson(jTicketClass => for {
      code <- (jTicketClass --\ "Code").read[String]
      name <- (jTicketClass --\ "Name").read[String]
    } yield TicketClass(code, name))
}
