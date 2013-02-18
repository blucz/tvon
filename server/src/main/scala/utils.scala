package tvon.server;

import java.util.UUID

trait Lock {
  val _lock = new Object
  def lock[A](code: => A): A = {
    _lock.synchronized { code }
  }
}

trait CancelationToken {
  def cancel() : Unit
}

object Utils {
  def newGuid(): String = UUID.randomUUID.toString.replace("-", "")
  def normalizedEquals(s: String, t: String): Boolean = {
    // XXX: unicode cleanup, etc
    s.trim.equalsIgnoreCase(t.trim)
  }
  def generateKey(s:String): String = {
    s.toLowerCase().map(c => c match {
      case c if c.isLetterOrDigit => c
      case _                      => '_'
    })
  }
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


