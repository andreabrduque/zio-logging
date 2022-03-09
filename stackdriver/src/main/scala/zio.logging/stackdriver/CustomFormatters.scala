package zio.logging.stackdriver

import zio.logging.LogFormat.{ annotation, label, text }
import zio.logging.internal.LogAppender
import zio.logging.{ LogFormat, json }
import zio.{ Cause, RuntimeConfigAspect, ZTraceElement }

import java.time.ZonedDateTime

private[stackdriver] object CustomFormatters {

  def escape(value: String): String = "\"" + value + "\""

  private val openCurlyBrace  = text("{")
  private val closeCurlyBrace = text("}")
  private val comma           = text(",")

  private val timestamp =
    text {
      val now = ZonedDateTime.now()
      now.toInstant.toEpochMilli.toString
    }

  private val enclosingClass: LogFormat =
    LogFormat.make { (builder, trace, _, _, _, _, _, _, _) =>
      trace match {
        case ZTraceElement(_, file, _) => builder.appendText(escape(file))
        case _                         => builder.appendText("not-available")
      }
    }

  private val fiberId: LogFormat =
    LogFormat.make { (builder, _, fiberId, _, _, _, _, _, _) =>
      builder.appendText(escape(fiberId.threadName))
    }

  private val level = LogFormat.make { (builder, _, _, level, _, _, _, _, _) =>
    builder.appendText(escape(level.label))
  }

  private def customAppender(textAppender: String => Any): LogAppender = new LogAppender { self =>
    override def appendCause(cause: Cause[Any]): Unit = appendText(cause.prettyPrint)

    override def appendNumeric[A](numeric: A): Unit = appendText(numeric.toString)

    override def appendText(text: String): Unit = { textAppender(text); () }

    override def closeKeyOpenValue(): Unit = appendText(":")

    override def closeValue(): Unit = ()

    override def openKey(): Unit = ()

  }

  def getLogger(): RuntimeConfigAspect = {
    val builder                   = new StringBuilder()
    val effectful: String => Unit = (x: String) => builder.append(x)
    val appender: LogAppender     = customAppender(effectful(_))

    val custom: LogFormat =
      openCurlyBrace + label("\"timestamp\"", timestamp) + comma + label(
        escape("severity"),
        level
      ) + comma + label(
        escape("thread"),
        fiberId
      ) + comma + label(
        escape("logger"),
        enclosingClass
      ) + comma + annotation(escape("context")) + comma + label(
        escape("message"),
        LogFormat.line
      ) + closeCurlyBrace + LogFormat.newLine

    json(
      appender,
      (_: Any) => {
        val result = builder.toString()
        builder.clear()
        result
      },
      custom
    )
  }

}
