package tvon.server;

import java.util.UUID
import java.text.Normalizer
import scala.util.matching.Regex
import scala.collection._

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
  def generateUrlSafeKey(s:String): String = {
    s.toLowerCase().map(c => c match {
      case c if c.isLetterOrDigit => c
      case _                      => '_'
    })
  }
  def getMergeKey(raw:String) = {
    var base = raw.trim.toLowerCase.filter(!_.isWhitespace)
    Normalizer.normalize(base, Normalizer.Form.NFD);
  }
  def getSortKey(raw:String) = {
    var base = raw.trim.toLowerCase
    Normalizer.normalize(base, Normalizer.Form.NFD);
  }

  def htmlDecode(s:String):String = {
    // XXX: handle hex
    s.replace("&amp;", "&")
     .replace("&lt;", "<")
     .replace("&gt;", ">")
     .replace("&apos;", "'")
     .replace("&quot;", "\"")
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

object Extensions {
  implicit class RichList[A](l: List[A]) {
    def distinctWith[B](f: (A) => B): List[A] = {
      val seen = mutable.HashMap[B,A]()
      var b    = List[A]()
      for (a <- l) {
        val k = f(a)
        if (!seen.contains(k)) {
          b = a :: b
          seen(k) = a
        }
      }
      b.reverse
    }
  }

  implicit class RichIterable[A](i: Iterable[A]) {
    def tryPick[B](picker: (A) => Option[B]) : Option[B] = {
      for (a <- i) {
        picker(a) match {
          case None    => ()
          case Some(b) => return Some(b)
        }
      }
      return None
    }
  }

  implicit class RichString(s: String) {
    def iterateUntilClean(cleaner: (String) => String): String = {
      val orig = s
      var last = orig
      var curr = orig
      do {
        last = curr
        curr = cleaner(curr)
      } while (curr != last)
      curr
    }

    def clean(a: String, b: String): String = {
      s.iterateUntilClean(s => s.replace(a,b))
    }

    def remove(m: Regex.Match, replacewith: String): String = s.substring(0, m.start) + replacewith + s.substring(m.end)

    def cleanSuffix(suffixes: Iterable[String]): String = {
      s.iterateUntilClean(s => suffixes.foldLeft(s)((a,suffix) => a.stripSuffix(suffix)))
    }
    def cleanPrefix(prefixes: Iterable[String]): String = {
      s.iterateUntilClean(s => prefixes.foldLeft(s)((a,prefix) => a.stripPrefix(prefix)))
    }
  }
}
