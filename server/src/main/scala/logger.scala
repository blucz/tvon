package tvon.server

object Log {
  import org.slf4j.LoggerFactory
  lazy val log = LoggerFactory.getLogger("tvon_server")
  def debug  (s:String) { log.debug(s) }
  def info   (s:String) { log.info(s)  }
  def warning(s:String) { log.warn(s)  }
  def warn   (s:String) { log.warn(s)  }
  def error  (s:String) { log.error(s) }
  def trace  (s:String) { log.trace(s) }
  def warning(s:String, e:Throwable) { 
    // XXX: print stack trace
    warning(s + ": " + e);
  }
  def error(s:String, e:Throwable) { 
    // XXX: print stack trace
    error(s + ": " + e);
  }
}

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase
class LogColorize extends ForegroundCompositeConverterBase[ILoggingEvent] {
  import ch.qos.logback.classic.Level
  import ch.qos.logback.core.pattern.color.ANSIConstants._

  protected override def getForegroundColorCode(event: ILoggingEvent): String = {
    event.getLevel.toInt match {
      case Level.ERROR_INT => BOLD+RED_FG
      case Level.WARN_INT  => RED_FG
      case Level.INFO_INT  => BOLD+WHITE_FG
      case Level.TRACE_INT => CYAN_FG
      case Level.DEBUG_INT => GREEN_FG
      case _               => DEFAULT_FG
    }
  }
}
