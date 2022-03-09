import zio.{ Cause, ExitCode, FiberId, LogLevel, LogSpan, RuntimeConfigAspect, ZFiberRef, ZLogger, ZTraceElement }
import zio.logging.{ LogAnnotation, LogColor, LogContext, LogFormat, console, json, logContext }
import zio.logging.LogFormat.{ annotation, fiberId, label, level, line, quoted, text, timestamp }

import java.{ util => ju }
import zio.json._
import zio.json.internal._
import zio.json.ast._
import zio.logging.internal.LogAppender
import zio._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer

object Example {

  /**
   * A [[LogAppender]] for unstructured logging, which simply turns everything
   * into text, and passes it to the given text appender function.
   */
  def structured(jsonAppender: String => Any): LogAppender = new LogAppender { self =>
    override def appendCause(cause: Cause[Any]): Unit = appendText(cause.prettyPrint)

    override def appendNumeric[A](numeric: A): Unit = appendText(numeric.toString)

    override def appendText(text: String): Unit = { jsonAppender(text); () }

    override def closeKeyOpenValue(): Unit = appendText(":")

    override def closeValue(): Unit = ()

    override def openKey(): Unit = ()

  }

  def label(label: => String, value: LogFormat): LogFormat =
    LogFormat.make { (builder, trace, fiberId, logLevel, message, context, spans, location, annotations) =>
      builder.openKey()
      try builder.appendText(label)
      finally builder.closeKeyOpenValue()

      try value.unsafeFormat(builder)(trace, fiberId, logLevel, message, context, spans, location, annotations)
      finally builder.closeValue()
    }

  //custom formatters
  def timestamp(): LogFormat =
    text {
      val now = ZonedDateTime.now()
      now.toInstant.toEpochMilli.toString
    }

  val enclosingClass: LogFormat =
    LogFormat.make { (builder, trace, _, _, _, _, _, _, _) =>
      trace match {
        case ZTraceElement(_, file, _) => builder.appendText(s"\"${file}\"")
        case _                         => builder.appendText("not-available")
      }
    }

  val fiberId: LogFormat =
    LogFormat.make { (builder, _, fiberId, _, _, _, _, _, _) =>
      builder.appendText(s"\"${fiberId.threadName}\"")
    }

  val level =  LogFormat.make { (builder, _, _, level, _, _, _, _, _) =>
    builder.appendText(s"\"${level.label}\"")
  }


  def getLogger(): RuntimeConfigAspect = {
    val builder                   = new StringBuilder()
    val effectful: String => Unit = (x: String) => builder.append(x)
    val appender: LogAppender     = structured(effectful(_))

    //make logformatters: comma, openBrace, closeBrace

    val custom: LogFormat =
      LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
        builder.appendText("{")
      } + annotation("\"context\"") + LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
        builder.appendText(",")
      } + label(
        "\"message\"",
        LogFormat.make { (builder, _, _, _, line, _, _, _, _) =>
          builder.appendText(line())
        }
      ) + text(",") + label("\"timestamp\"", timestamp()) + text(",") + label(
        "\"severity\"",
        level
      ) + text(",") + label(
        "\"thread\"",
        fiberId
      ) + text(",") + label(
        "\"logger\"",
        enclosingClass
      ) + LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
        builder.appendText("}")
      } + LogFormat.newLine

    val meh = json(
      appender,
      (_: Any) => {
        val result = builder.toString()
        builder.clear()
        result
      },
      custom
    )

    meh
  }

}

final case class LoggingAnnotation[A](key: String, value: A)(implicit encoder: JsonEncoder[A]) {
  def encode() = s"{\"${key}\": \"${value.toJson}\"}"
}

sealed trait Logging {

  private def escape(value: String) = "\"" + value + "\""

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

  //function make or build
}

object App extends ZIOAppDefault {

  //timestamp
  //severity
  //thread
  //logger

  object LoggingLive extends Logging

  import zio.logging._

  override def hook: RuntimeConfigAspect = Example.getLogger()

  import zio._
  //test LoggingAnnotation with other objects
  override def run = LoggingLive.warn(LoggingAnnotation("size", 10), "the payload is too big")

}
