package zio.logging.stackdriver

import zio.json._
import zio.json.internal._
import zio.{ UIO, ZIO }
import zio.RuntimeConfigAspect

final case class LoggingAnnotation[A](key: String, value: A)(implicit encoder: JsonEncoder[A]) {
  def encode(): String = s"{\"${key}\": \"${value.toJson}\"}"
}

object Logging {

  import zio.logging.stackdriver.CustomFormatters._

  private def annotateEffect(value: String): ZIO.LogAnnotate = ZIO.logAnnotate(escape("context"), value)

  def error[A](context: LoggingAnnotation[A], message: String)(implicit encoder: JsonEncoder[A]): UIO[Unit] =
    annotateEffect(context.encode()) {
      ZIO.logError(escape(message))
    }
  def info[A](context: LoggingAnnotation[A], message: String)(implicit encoder: JsonEncoder[A]): UIO[Unit]  =
    annotateEffect(context.encode()) {
      ZIO.logInfo(escape(message))
    }
  def warn[A](context: LoggingAnnotation[A], message: String)(implicit encoder: JsonEncoder[A]): UIO[Unit]  =
    annotateEffect(context.encode()) {
      ZIO.logWarning(escape(message))
    }
  def debug[A](context: LoggingAnnotation[A], message: String)(implicit encoder: JsonEncoder[A]): UIO[Unit] =
    annotateEffect(context.encode()) {
      ZIO.logDebug(escape(message))
    }

  def make(): RuntimeConfigAspect = getLogger()

}
