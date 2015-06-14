package utils

import com.github.nscala_time.time.DurationBuilder
import java.util.{Timer, TimerTask}
import com.github.nscala_time.time.Imports._
import org.joda.time.{Period, LocalDate}

object After {
  def apply[T](duration: DurationBuilder)(action: => T): Unit = timer.schedule(asTimerTask(action), duration.millis)

  private val timer = new Timer
  private def asTimerTask(action: => Unit) = new TimerTask { override def run() = action }
}

object Log {
  def apply(msg: String) : Unit = println(s"${LocalDateTime.now.toString(DateTimeFormat.longDateTime())}: $msg")
}

case class SplitAt(n: Int) {
  def unapply[T](s: Seq[T]): Option[(Seq[T], Seq[T])] =
    if (s.isEmpty) None
    else Some(s.splitAt(n))
}

object Implicits {
  implicit class RichLocalDate(date: LocalDate) {
    def to(endDate: LocalDate): (Period) => Iterator[LocalDate] = step => Iterator.iterate(date)(_.plus(step)).takeWhile(_ <= endDate)
  }
  implicit class RichLocalDateIterator(filter: Period => Iterator[LocalDate]) {
    def withStep(period: Period) = filter(period)
  }
}