package api.OneTwoTrip

import java.time._
import java.time.format.DateTimeFormatter

import spray.json._

import scala.util.Try

object JsonProtocol extends DefaultJsonProtocol {

  def parse(searchResponse: String): Try[SearchResponse] = Try {
    val json      = searchResponse.parseJson

    val jPlanes   = fromField[JsObject]       (json, "planes")
    val jRates    = fromField[JsObject]       (json, "rates")
    val jTrips    = fromField[Vector[JsValue]](json, "trps")
    val jFares    = fromField[Vector[JsValue]](json, "frs")

    val fares = jFares.map(readFare)
    val trips = jTrips.map(readTrip)
    val planes = readPlanes(jPlanes)
    val rates = readRates(jRates)

    SearchResponse(fares, trips, planes, rates)
  }

  private def readPlanes(planes: JsObject): Seq[Plane] = {
    val p = planes.fields map {
      case (code, JsString(name)) => Plane(code, name)
    }

    p.toSeq
  }

  private def readRates(rates: JsObject): Seq[Rate] = {
    val r = rates.fields map {
      case (currencyPair, JsString(rate)) => currencyPair.splitAt(3) match {
        case (from, to) => Rate(from, to, rate.toDouble)
      }
    }

    r.toSeq
  }

  private def readTrip(trip: JsValue): Trip = {
    val stDt    = fromField[String]         (trip, "stDt")
    val stTm    = fromField[String]         (trip, "stTm")
    val from    = fromField[String]         (trip, "from")
    val to      = fromField[String]         (trip, "to")
    val airCmp  = fromField[String]         (trip, "airCmp")
    val fltNm   = fromField[String]         (trip, "fltNm")
    val oprdBy  = fromField[Option[String]] (trip, "oprdBy")
    val plane   = fromField[String]         (trip, "plane")

    val dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
    val timeFormatter = DateTimeFormatter.ofPattern("HHmm")

    val date = LocalDate.parse(stDt, dateFormatter)
    val time = LocalTime.parse(stTm, timeFormatter)

    Trip(date, time, from, to, airCmp, fltNm, oprdBy, plane)
  }

  private def readFare(fare: JsValue): Fare = {
    val id      = fromField[Int](fare, "id")
    val prcInf  = fromField[JsValue](fare, "prcInf")
    val dirs    = fromField[Vector[JsValue]](fare, "dirs")

    val directions = dirs.map(dir => readDirection(dir))
    val priceInfo = readPriceInfo(prcInf)

    Fare(id, directions, priceInfo)
  }

  private def readDirection(direction: JsValue): Direction = {
    val id = fromField[Int](direction, "id")
    val trps = fromField[Vector[JsValue]](direction, "trps")

    val trips = trps.map(trip => readTripRef(trip))

    Direction(id, trips)
  }

  private def readTripRef(trip: JsValue): TripRef = {
    val id      = fromField[Int]    (trip, "id")
    val cls     = fromField[String] (trip, "cls")
    val srvCls  = fromField[String] (trip, "srvCls")

    TripRef(id, cls, srvCls)
  }

  private def readPriceInfo(value: JsValue): PriceInfo = {
    val adtB      = fromField[BigDecimal](value, "adtB")
    val adtT      = fromField[BigDecimal](value, "adtT")
    val markupNew = fromField[BigDecimal](value, "markupNew")
    val cur       = fromField[String]    (value, "cur")

    PriceInfo(adtB, adtT, cur, markupNew)
  }
}