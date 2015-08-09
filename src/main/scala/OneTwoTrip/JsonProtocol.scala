package OneTwoTrip

import java.time._
import java.time.format.DateTimeFormatter

import spray.json._

import scala.util.Try

object JsonProtocol extends DefaultJsonProtocol {

  private case class Rate(currencyFrom: String, currencyTo: String, factor: BigDecimal)

  private case class PriceInfo(adultFare: BigDecimal, adultTaxes: BigDecimal, currency: String, markup: BigDecimal) {
    val totalPrice = adultFare + adultTaxes + markup
  }

  def parse(searchResponse: String): Try[Seq[Fare]] = Try {
    val json      = searchResponse.parseJson

    val jPlanes   = fromField[JsObject]       (json, "planes")
    val jRates    = fromField[JsObject]       (json, "rates")
    val jTrips    = fromField[Vector[JsValue]](json, "trps")
    val jFares    = fromField[Vector[JsValue]](json, "frs")

    val planes    = readPlanes(jPlanes)
    val rates     = readRates(jRates)

    val trips = jTrips.map(trip => readTripTemplate(trip, planes))

    jFares.map(fare => readFare(fare, trips, rates)).flatMap(f => f.right.toOption)
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

  private def readTripTemplate(trip: JsValue, planes: Seq[Plane]): Either[String, Segment] = try {
    val stDt    = fromField[String]         (trip, "stDt")
    val stTm    = fromField[String]         (trip, "stTm")
    val from    = fromField[String]         (trip, "from")
    val to      = fromField[String]         (trip, "to")
    val airCmp  = fromField[String]         (trip, "airCmp")
    val fltNm   = fromField[String]         (trip, "fltNm")
    val oprdBy  = fromField[Option[String]] (trip, "oprdBy")
    val plane   = fromField[String]         (trip, "plane")

    val formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmm")
    val departureLocalTime = LocalDateTime.parse(stDt + stTm, formatter)
    val departureOffsetTime = OffsetDateTime.of(departureLocalTime, timeZoneOffsets(from))

    val segment = Segment(
      departureDate     = departureOffsetTime,
      fromAirport       = from,
      toAirport         = to,
      airline           = airCmp,
      flightNumber      = fltNm,
      operatedBy        = oprdBy,
      plane             = planes.find(p => p.code == plane).head.name,
      reservationClass  = "",
      cabinClass        = "")

    Right(segment)

  } catch {
    case ex: Throwable => Left(ex.getMessage)
  }

  private def readFare(fare: JsValue, segments: Seq[Either[String, Segment]], rates: Seq[Rate]): Either[String, Fare] = try {
    val prcInf  = fromField[JsValue](fare, "prcInf")
    val dirs    = fromField[Vector[JsValue]](fare, "dirs")

    val directions = dirs.map(dir => readDirection(dir, segments))
    val priceInfo = readPriceInfo(prcInf)

    if (directions.count(_.isLeft) == 0) {
      val usdPrice = getUSDValue(priceInfo.totalPrice, priceInfo.currency, rates)
      Right(Fare(directions.map(_.right.get), usdPrice, OffsetDateTime.now))
    } else {
      Left(directions.flatMap(d => d.left.toOption).mkString("\n"))
    }

  } catch {
    case ex: Throwable => Left(ex.getMessage)
  }

  private def readDirection(direction: JsValue, tripTemplates: Seq[Either[String, Segment]]): Either[String, Flight] = try {
    val trps = fromField[Vector[JsValue]](direction, "trps")

    val trips = trps.map(trip => readTrip(trip, tripTemplates))

    if (trips.count(_.isLeft) == 0)
      Right(Flight(trips.map(_.right.get)))
    else
      Left(trips.flatMap(t => t.left.toOption).mkString("\n"))

  } catch {
    case ex: Throwable => Left(ex.getMessage)
  }

  private def readTrip(trip: JsValue, tripTemplates: Seq[Either[String, Segment]]): Either[String, Segment] = try {
    val id      = fromField[Int]    (trip, "id")
    val cls     = fromField[String] (trip, "cls")
    val srvCls  = fromField[String] (trip, "srvCls")

    tripTemplates(id).right.map(_.copy(reservationClass = cls, cabinClass = srvCls))
  } catch {
    case ex: Throwable => Left(ex.getMessage)
  }

  private def readPriceInfo(value: JsValue): PriceInfo = {
    val adtB      = fromField[BigDecimal](value, "adtB")
    val adtT      = fromField[BigDecimal](value, "adtT")
    val markupNew = fromField[BigDecimal](value, "markupNew")
    val cur       = fromField[String]    (value, "cur")

    PriceInfo(adtB, adtT, cur, markupNew)
  }

  private def getUSDValue(value: BigDecimal, currentCurrency: String, rates: Seq[Rate]) = currentCurrency match {
    case "USD" => value
    case currency =>
      val rate = rates.find(r => r.currencyFrom == currency && r.currencyTo == "USD").head

      value * rate.factor
  }

  val timeZoneOffsets = flights.ReferenceData.airports.map(a => (a.iataCode, a.zoneOffset)).toMap
}