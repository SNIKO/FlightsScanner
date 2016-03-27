package utils

import java.io.PrintWriter
import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.{Timer, TimerTask}

import argonaut.Argonaut._
import argonaut._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scalaz._

object Utils {

  def saveToFile(filePath: String, content: String): Future[String] = Future {
    val writer = new PrintWriter(filePath)
    writer.write(content)
    writer.close()
    filePath
  }

  type ActionResult[TError, TResult] = TError \/ TResult
  type FutureActionResult[TError, TResult] = Future[ActionResult[TError, TResult]]
  val ActionSuccess = \/-
  val ActionFailure = -\/
}

object After {
  def apply[T](duration: Duration)(action: => Future[T]) = {
    val promise = Promise[T]()

    timer.schedule(
      new TimerTask {
        override def run() = {
          promise.completeWith(action)
        }
      },
      duration.getSeconds * 1000)

    promise.future
  }

  private val timer = new Timer
}

object Log {
  def apply(msg: String): Unit = println(s"${LocalDateTime.now.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}: $msg")
}

case class SplitAt(n: Int) {
  def unapply[T](s: Seq[T]): Option[(Seq[T], Seq[T])] =
    if (s.isEmpty) None
    else Some(s.splitAt(n))
}

object Implicits {

  implicit class RichLocalDate(date: LocalDate) {
    def isBeforeOrEqual(other: LocalDate) = date.isBefore(other) || date.isEqual(other)
    
    def to(endDate: LocalDate): Period => Iterator[LocalDate] = {
      step => Iterator.iterate(date)(_.plus(step)).takeWhile(_.isBeforeOrEqual(endDate))
    }
  }

  implicit class RichLocalDateIterator(filter: Period => Iterator[LocalDate]) {
    def withStep(period: Period) = filter(period)
  }

  implicit class RichDateTimeFormatter(formatter: DateTimeFormatter) {
    def withZone(zoneId: Option[String]): DateTimeFormatter = zoneId match {
      case Some(id) => formatter.withZone(ZoneId.of(id))
      case None => formatter
    }
  }

  implicit class RichStringBuilder(stringBuilder: StringBuilder) {
    def appendLine(s: String): StringBuilder = stringBuilder.append(s).append("\n")
  }

  implicit class RichACursor(cursor: ACursor) {
    def asLocalDateTime(formatter: DateTimeFormatter): DecodeResult[LocalDateTime] = {
      try {
        cursor.as[String].map(d => LocalDateTime.parse(d, formatter))
      } catch {
        case e: DateTimeParseException => DecodeResult.fail(s"Failed to decode json: ${cursor.focus}. ${e.getMessage}", cursor.history)
      }
    }

    def readLocalDate(formatter: DateTimeFormatter): DecodeResult[LocalDate] = {
      try {
        cursor.read[String].map(d => LocalDate.parse(d, formatter))
      } catch {
        case e: DateTimeParseException => DecodeResult.fail(s"Failed to decode json: ${cursor.focus}. ${e.getMessage}", cursor.history)
      }
    }

    def readLocalTime(formatter: DateTimeFormatter): DecodeResult[LocalTime] = {
      try {
        cursor.read[String].map(d => LocalTime.parse(d, formatter))
      } catch {
        case e: DateTimeParseException => DecodeResult.fail(s"Failed to decode json: ${cursor.focus}. ${e.getMessage}", cursor.history)
      }
    }

    def readDuration(): DecodeResult[Duration] = {
      try {
        cursor.read[String].map(d => d.splitAt(2) match {
          case (hh, mm) => Duration.ofSeconds(hh.toInt * 3600 + mm.toInt * 60)
        })
      } catch {
        case e => DecodeResult.fail(s"Failed to decode json: ${cursor.focus}. ${e.getMessage}", cursor.history)
      }
    }

    def read[T](implicit e: DecodeJson[T]): DecodeResult[T] =
      cursor.as[T] match {
        case DecodeResult(-\/(err)) =>
          val failureFocus = if (cursor.failed) cursor.failureFocus else cursor.focus
          DecodeResult.fail(s"Failed to decode json: $failureFocus. ${err._1}", err._2)
        case res => res
      }

    def readArrayOf[T](implicit e: DecodeJson[T]): DecodeResult[Vector[T]] =
      cursor.succeeded match {
        case true => cursor.focus match {
          case None => DecodeResult.ok(Vector.empty[T])
          case Some(j) if j.isNull => DecodeResult.ok(Vector.empty[T])
          case Some(j) if j.isArray => j.acursor.as[Vector[T]]
          case Some(j) => j.acursor.as[T].map(r => Vector(r))
        }
        case false => DecodeResult.fail(s"Failed to decode json: ${cursor.failureFocus}. Expected element not found:", cursor.history)
      }

    def tryRead[T](implicit e: DecodeJson[T]): DecodeResult[Option[T]] =
      cursor.succeeded match {
        case true => cursor.focus match {
          case None => DecodeResult.ok(None)
          case Some(j) if j.isNull => DecodeResult.ok(None)
          case Some(j) => j.acursor.read[T].map(Some(_))
        }
        case false => DecodeResult.ok(None)
      }
  }

  implicit val FutureMonad = new Monad[Future] {
    def point[A](a : => A): Future[A] = Future(a)
    def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa flatMap f
  }
}