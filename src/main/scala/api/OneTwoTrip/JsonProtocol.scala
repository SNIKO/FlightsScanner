package api.OneTwoTrip

import java.time._
import java.time.format.DateTimeFormatter

import argonaut.Argonaut._
import argonaut._

import utils.Implicits._

object JsonProtocol {
  /**
   * SearchResponse
   */
  case class SearchResponse(fares: Seq[Fare], trips: Seq[Trip], planes: Seq[Plane],rates: Seq[Rate])

  object SearchResponse {
    implicit def Decoder: DecodeJson[SearchResponse] =
      DecodeJson(root => for {
        planes <- (root --\ "planes").read[PlanesList]
        rates <- (root --\ "rates").read[RatesList]
        trips <- (root --\ "trps").readArrayOf[Trip]
        fares <- (root --\ "frs").readArrayOf[Fare]
      } yield SearchResponse(fares, trips, planes.planes, rates.rates))
  }

  /**
   * Planes
   */
  case class Plane(code: String, name: String)
  case class PlanesList(planes: Seq[Plane])

  object PlanesList {
    implicit def Decoder: DecodeJson[PlanesList] =
      DecodeJson(jPlanes => for {
        pairs <- jPlanes.as[Map[String, String]]
        planes = pairs.map { case (code, name) => Plane(code, name) }
      } yield PlanesList(planes.toSeq))
  }

  /**
   * Rates
   */
  case class Rate(currencyFrom: String, currencyTo: String, factor: String)
  case class RatesList(rates: Seq[Rate])

  object RatesList {
    implicit def Decoder: DecodeJson[RatesList] =
      DecodeJson(jRates => for {
        pairs <- jRates.as[Map[String, String]]
        rates = pairs.map {
          case (currencyPair, rate) => currencyPair.splitAt(3) match {
            case (from, to) => Rate(from, to, rate)
          }
        }
      } yield RatesList(rates.toSeq))
  }

  /**
   * Trip
   */
  case class Trip(date: LocalDate, time: LocalTime, from: String, to: String, airline: String, flightNumber: String, operatedBy: Option[String], plane: String)

  object Trip {
    val dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    val timeFormatter = DateTimeFormatter.ofPattern("HHmm")

    implicit def Decoder: DecodeJson[Trip] =
      DecodeJson(jTrip => for {
          stDt <- (jTrip --\ "stDt").readLocalDate(dateFormatter)
          stTm <- (jTrip --\ "stTm").readLocalTime(timeFormatter)
          from <- (jTrip --\ "from").read[String]
            to <- (jTrip --\ "to").read[String]
        airCmp <- (jTrip --\ "airCmp").read[String]
         fltNm <- (jTrip --\ "fltNm").read[String]
        oprdBy <- (jTrip --\ "oprdBy").tryRead[String]
         plane <- (jTrip --\ "plane").read[String]
      } yield Trip(stDt, stTm, from, to, airCmp, fltNm, oprdBy, plane))
  }

  /**
   * Fare
   */
  case class Fare(id: Int, directions: Seq[Direction], priceInfo: PriceInfo)

  object Fare {
    implicit def Decoder: DecodeJson[Fare] =
      DecodeJson(jFare => for {
            id <- (jFare --\ "id").read[Int]
        prcInf <- (jFare --\ "prcInf").read[PriceInfo]
          dirs <- (jFare --\ "dirs").readArrayOf[Direction]
      } yield Fare(id, dirs, prcInf))
  }

  /**
   * Direction
   */
  case class Direction(id: Int, trips: Seq[TripRef])

  object Direction {
    implicit def Decoder: DecodeJson[Direction] =
      DecodeJson(jDirection => for {
          id <- (jDirection --\ "id").read[Int]
        trps <- (jDirection --\ "trps").readArrayOf[TripRef]
      } yield Direction(id, trps))
  }

  /**
   * TripRef
   */
  case class TripRef(id: Int, reservationClass: String, cabinClass: String)

  object TripRef {
    implicit def Decoder: DecodeJson[TripRef] =
      DecodeJson(jTripRef => for {
            id <- (jTripRef --\ "id").read[Int]
           cls <- (jTripRef --\ "cls").read[String]
        srvCls <- (jTripRef --\ "srvCls").read[String]
      } yield TripRef(id, cls, srvCls))
  }

  /**
   * PriceInfo
   */
  case class PriceInfo(adultFare: BigDecimal, adultTaxes: BigDecimal, currency: String, markup: BigDecimal)

  object PriceInfo {
    implicit def Decoder: DecodeJson[PriceInfo] =
      DecodeJson(jPriceInfo => for {
        adtB      <- (jPriceInfo --\ "adtB").read[Float]
        adtT      <- (jPriceInfo --\ "adtT").read[Float]
        markupNew <- (jPriceInfo --\ "markupNew").read[Float]
        cur       <- (jPriceInfo --\ "cur").read[String]
      } yield PriceInfo(adtB.toDouble, adtT.toDouble, cur, markupNew.toDouble))
  }
}
