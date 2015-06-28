package OneTwoTrip

import com.github.nscala_time.time.Imports._
import spray.json.{DefaultJsonProtocol, JsString, JsValue, _}

import scala.util.Try

object JsonProtocol extends DefaultJsonProtocol {

  private case class Rate(currencyFrom: String, currencyTo: String, factor: BigDecimal)

  private case class PriceInfo(adultFare: BigDecimal, adultTaxes: BigDecimal, currency: String, markup: BigDecimal)

  def parse(searchResponse: String): Try[Seq[Fare]] = Try {
    val json = searchResponse.parseJson
    val jPlanes = fromField[JsObject](json, "planes")
    val jRates = fromField[JsObject](json, "rates")
    val jTrips = fromField[Vector[JsValue]](json, "trps")
    val jFares = fromField[Vector[JsValue]](json, "frs")

    val planes = readPlanes(jPlanes)
    val rates = readRates(jRates)
    val segments = readSegments(jTrips, planes)

    readFares(jFares, segments, rates)
  }

  private def readPlanes(planes: JsObject): Seq[Plane] = {
    val p = planes.fields map {
      case (code, JsString(name)) => Plane(code, name)}

    p.toSeq
  }

  private def readRates(rates: JsObject) = {
    val r = rates.fields map {
      case (currencyPair, JsString(rate)) => currencyPair.splitAt(3) match {
        case (from, to) => Rate(from, to, rate.toDouble)}}

    r.toSeq
  }

  private def readSegments(trips: Seq[JsValue], planes: Seq[Plane]) = {
    trips map (trip => {
        val stDt    = fromField[String](trip, "stDt")
        val stTm    = fromField[String](trip, "stTm")
        val from    = fromField[String](trip, "from")
        val to      = fromField[String](trip, "to")
        val airCmp  = fromField[String](trip, "airCmp")
        val fltNm   = fromField[String](trip, "fltNm")
        val oprdBy  = fromField[Option[String]](trip, "oprdBy")
        val plane   = fromField[String](trip, "plane")

        Segment(
          departureDate = DateTime.parse(stDt + stTm, DateTimeFormat.forPattern("yyyyMMddHHmm")),
          fromAirport = from,
          toAirport = to,
          airline = airCmp,
          flightNumber = fltNm,
          operatedBy = oprdBy,
          plane = planes.find(p => p.code == plane).head.name,
          reservationClass = "",
          cabinClass = "")
      })
  }

  private def readFares(fares: Seq[JsValue], segments: Seq[Segment], rates: Seq[Rate]) = {
      fares map (fare => {
        val prcInf = fromField[JsValue](fare, "prcInf")
        val dirs = fromField[Vector[JsValue]](fare, "dirs")

        val flights = dirs.map(direction => readFlight(direction, segments))

        val priceInfo = readPriceInfo(prcInf)
        val price = priceInfo.adultFare + priceInfo.adultTaxes + priceInfo.markup
        val usdPrice = priceInfo.currency match {
          case "USD" => price
          case currency => price * rates.find(r => r.currencyFrom == currency && r.currencyTo == "USD").head.factor
        }

        Fare(flights, usdPrice)
      })
  }

  private def readFlight(value: JsValue, segmentTemplates: Seq[Segment]) = {
    val segments = fromField[Vector[JsValue]](value, "trps") map (trp => {
      val id      = fromField[Int](trp, "id")
      val cls     = fromField[String](trp, "cls")
      val srvCls  = fromField[String](trp, "srvCls")

      segmentTemplates(id).copy(reservationClass = cls, cabinClass = srvCls)
    })

    Flight(segments)
  }

  private def readPriceInfo(value: JsValue): PriceInfo = {
    val adtB      = fromField[BigDecimal](value, "adtB")
    val adtT      = fromField[BigDecimal](value, "adtT")
    val markupNew = fromField[BigDecimal](value, "markupNew")
    val cur       = fromField[String](value, "cur")

    PriceInfo(adtB, adtT, cur, markupNew)
  }
}