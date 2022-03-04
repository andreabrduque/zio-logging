import com.fasterxml.jackson.annotation.JsonFormat
import zio.{Cause, ExitCode, FiberId, LogLevel, LogSpan, RuntimeConfigAspect, ZFiberRef, ZLogger, ZTraceElement}
import zio.logging.{LogAnnotation, LogColor, LogContext, LogFormat, console, json, logContext}
import zio.logging.LogFormat.{annotation, fiberId, label, level, line, quoted, timestamp}

import java.{util => ju}
import zio.json._
import zio.json.internal._
import zio.json.ast._
import zio.logging.internal.LogAppender
import zio._

import scala.collection.mutable.ListBuffer


object KeyJsonAnnotation {

  def apply(key: String): LogAnnotation[Either[String, Json]] =  LogAnnotation[Either[String, Json]](
    name = key,
    combine = (a: Either[String, Json], b: Either[String, Json]) => mergeAnnotation(a, b),
    render = (value: Either[String, Json]) => value.map(_.toString).merge
  )

  def mergeAnnotation(a1: Either[String, Json], b1: Either[String, Json]): Either[String, Json] = {
    for {
      a <-  a1
      b <- b1
    } yield a.merge(b)
  }

}

//class CustomAppender(self: LogAppender) extends LogAppender {
//  def appendCause(cause: Cause[Any]): Unit = self.appendCause(cause)
//
//  def appendNumeric[A](numeric: A): Unit = self.appendNumeric(numeric)
//
//  def appendText(text: String): Unit = self.appendText(text)
//
//  def closeKeyOpenValue(): Unit = self.closeKeyOpenValue()
//
//  def closeValue(): Unit = self.closeValue()
//
//  def openKey(): Unit = self.openKey()
//}

object Example {

  def mergeAnnotation(a1: Either[String, Json], b1: Either[String, Json]): Either[String, Json] = {
    for {
      a <-  a1
      b <- b1
    } yield a.merge(b)
  }


  /**
   * A [[LogAppender]] for unstructured logging, which simply turns everything
   * into text, and passes it to the given text appender function.
   */
  def structured(jsonAppender: String => Any): LogAppender = new LogAppender  { self =>
   override def appendCause(cause: Cause[Any]): Unit = appendText(cause.prettyPrint)

    override def appendNumeric[A](numeric: A): Unit = appendText(numeric.toString)

    override def appendText(text: String): Unit =  { jsonAppender(text); () }

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

  val builder = new StringBuilder()
  val effectful: String => Unit = (x: String) => builder.append(x)
  val appender: LogAppender = structured(effectful(_))

  val custom: LogFormat =
    LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
      builder.appendText("{")
    } +  annotation("\"context\"") + LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
      builder.appendText(",")
    } + label("\"message\"", LogFormat.make { (builder, _, _, _, line, _, _, _, _) =>
      builder.appendText(line())
    } ) + LogFormat.make { (builder, _, _, _, _, _, _, _, _) =>
      builder.appendText("}")
    } + LogFormat.newLine



  val meh = json(appender, (_: Any) => {
    builder.toString()
  }, custom)

}

object App extends ZIOAppDefault {

  import zio.logging._

 override def hook: RuntimeConfigAspect = Example.meh

    import zio._

  //inject any context into json form
  //if always pass key,value string->string pair
 val effect =  ZIO.logAnnotate("\"context\"", "{\"apples\": \"sauce\"}"){
   for {
     _ <- ZIO.log("\"log 1\"")
     _ <- ZIO.log("\"log 2\"")
   } yield ()
 }


  override def run = effect
}
