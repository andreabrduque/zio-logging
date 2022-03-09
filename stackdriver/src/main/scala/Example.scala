import zio.logging.stackdriver.{ Logging, LoggingAnnotation }
import zio._

object App extends ZIOAppDefault {

  override def hook: RuntimeConfigAspect = Logging.make()

  override def run: UIO[Unit] = Logging.warn(LoggingAnnotation("size", 10), "the payload is too big")

}
