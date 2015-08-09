package utils

import java.io.PrintWriter
import java.time._
import java.time.format.DateTimeFormatter
import java.util.{Timer, TimerTask}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object Utils {

  def saveToFile(filePath: String, content: String): Future[String] = Future {
    val writer = new PrintWriter(filePath)
    writer.write(content)
    writer.close()
    filePath
  }
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
}