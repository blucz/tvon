package tvon.server;

import org.slf4j.LoggerFactory

trait CancelationToken {
  def cancel() : Unit
}

object Log {
  lazy val log = LoggerFactory.getLogger("tvon_server")
  def debug  (s:String) { log.debug(s) }
  def info   (s:String) { log.info(s)  }
  def warning(s:String) { log.warn(s)  }
  def warn   (s:String) { log.warn(s)  }
  def error  (s:String) { log.error(s) }
  def trace  (s:String) { log.trace(s) }
}

object ThreadPool {
  import java.util.concurrent.{Executors,ExecutorService}

  lazy val pool : ExecutorService = Executors.newCachedThreadPool()

  def run(action: () => Unit) {
    pool.submit(new Runnable() {
      def run() {
        try {
          action()
        } catch {
          case e:Throwable => Log.error(s"[threadpool] ${e}")
          e.printStackTrace
        }
      }
    })
  }

  def queueLongRunning(action: () => Unit) {
    new Thread(new Runnable() {
      def run() {
        try {
          action()
        } catch {
          case e:Throwable => Log.error(s"[threadpool] ${e}")
          e.printStackTrace
        }
      }
    }).start
  }
}

class IteratorWrapper[A](iter:java.util.Iterator[A]) {
    def foreach(f: A => Unit): Unit = {
        while(iter.hasNext){
          f(iter.next)
        }
    }
}

object Hex {
  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
}
