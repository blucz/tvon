package tvon.server;

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable._
import java.nio._
import java.util.concurrent.TimeUnit
import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import scala.collection.JavaConversions._

trait StorageFile {
  def key       : String
  def path      : Path   
  def modTime   : Long
  def length    : Option[Long]
  def signature : Option[String]
}

case class ExistingFile(key: String, modTime: Long)

object StorageBackend {
  abstract class Event
  case class FilesAdded   (backend: StorageBackend, files: List[StorageFile]) extends Event
  case class FilesModified(backend: StorageBackend, files: List[StorageFile]) extends Event
  case class FilesRemoved (backend: StorageBackend, files: List[StorageFile]) extends Event
  case class Online       (backend: StorageBackend)
  case class Offline      (backend: StorageBackend)
}

trait StorageBackend {
  def id : String
  def watch(existing: List[ExistingFile], listener: Actor) : CancelationToken
}

class DirectoryStorageBackend(config:DirectoryConfig, extensions: List[String]) extends StorageBackend {
  val dirs             = new HashMap[Path, Dir]      // path -> dir
  var rootdir: Dir     = null
  var listener : Actor = null
  var rootpath         = Paths.get(config.path).toAbsolutePath()
  var rootpathstring   = rootpath.toString()

  def id: String = config.id

  def watch(existing: List[ExistingFile], listener: Actor) : CancelationToken = {
    if (this.listener != null) {
      throw new IllegalStateException("dirstorage only supports one watch at a time")
    }
    this.listener = listener
    populate(existing)
    ThreadPool.queueLongRunning(watcher_thread)
    listener ! StorageBackend.Online(this)
    return new CancelationToken {
      def cancel() { DirectoryStorageBackend.this.listener = null }
    }
  }

  class File(var dir: Dir, var name: String, var path: Path, var modTime: Long) extends StorageFile {
    var cached           : Boolean             = false
    var cached_signature : Option[String]      = None
    var cached_length    : Option[Long]        = None

    def key       : String                = mkFileKey(path)
    def length    : Option[Long]          = { cached_length    }
    def signature : Option[String]        = { cached_signature }

    def ensureCached() {
      if (!cached) {
        var channel = Files.newByteChannel(path, StandardOpenOption.READ)

        try {
          import java.security.MessageDigest

          val chunk_size = 32768;
          val len = channel.size()

          if (len < 3 * chunk_size) {
            channel.position(0)
            val buf = ByteBuffer.allocate(len.asInstanceOf[Int] + 8)
            channel.read(buf)
            buf.putLong(len)
            val sha1 = MessageDigest.getInstance("SHA-1");
            cached_signature = Some(Hex.valueOf(sha1.digest(buf.array())))
          } else {
            val buf = ByteBuffer.allocate(chunk_size * 3 + 8)
            channel.position(0)
            buf.limit(chunk_size)
            channel.read(buf)
            channel.position(len / 2)
            buf.limit(chunk_size*2)
            channel.read(buf)
            channel.position(len - chunk_size)
            buf.limit(chunk_size*3)
            channel.read(buf)
            buf.limit(buf.capacity())
            buf.putLong(len)
            val sha1 = MessageDigest.getInstance("SHA-1");
            cached_signature = Some(Hex.valueOf(sha1.digest(buf.array())))
          }
          cached_length    = Some(len)

          //Log.info(s"path ${path} sig ${Hex.valueOf(signature.get)} len ${length}")

        } finally {
          channel.close()
        }
      }
    }

    def updateModTime(newmodtime: Long): Boolean = {
      if (newmodtime != modTime) {
        modTime  = newmodtime
        cached   = false
        return true
      } else {
        return false
      }
    }
  }

  //
  // Notification system 
  //
  val pending_removes = new ListBuffer[File]()
  val pending_adds    = new ListBuffer[File]()
  val pending_mods    = new ListBuffer[File]()

  private def queue_add(file: File)    { 
    try {
      file.ensureCached()
      pending_adds.append(file)
      if (pending_adds.size() >= 100) {
        flush_adds()
      }
    } catch {
      case e:Throwable => Log.error(s"[dirstorage] error caching file data: ${e}")
      e.printStackTrace
    }
  }

  private def queue_mod(file: File) { 
    try {
      file.ensureCached()
      pending_mods.append(file)
      if (pending_mods.size() >= 100) {
        flush_mods()
      }
    } catch {
      case e:Throwable => Log.error(s"[dirstorage] error caching file data: ${e}")
      e.printStackTrace
    }
  }

  private def queue_remove(file: File) { 
    pending_removes.append(file) 
    if (pending_removes.size() >= 100) {
      flush_removes()
    }
  }

  def flush_adds() {
    if (pending_adds.size() > 0) {
      listener ! StorageBackend.FilesAdded(this, pending_adds.toList)
      pending_adds.clear()
    }
  }

  def flush_removes() {
    if (pending_removes.size() > 0) {
      listener ! StorageBackend.FilesRemoved(this, pending_removes.toList)
      pending_removes.clear()
    }
  }

  def flush_mods() {
    if (pending_mods.size() > 0) {
      listener ! StorageBackend.FilesModified(this, pending_mods.toList)
      pending_mods.clear()
    }
  }

  def flush() {
    flush_removes()
    flush_adds()
    flush_mods()
  }

  class Dir(var parent: Option[Dir], var path: Path, var name: String) {
    var subdirs            = new HashSet[Dir]
    var files              = new HashSet[File]
    var watchkey: WatchKey = null

    def ensureWatch(watcher: WatchService) {
      if (watchkey == null) { 
        watchkey = path.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)
      }
    }

    def cancelWatch() {
      if (watchkey != null) {
        watchkey.cancel()
        watchkey = null
      }
    }

    def getOrCreateSubDir(path: Path, name: String) : Dir = {
      return subdirs.find(_.name == name) match {
        case Some(x) => x
        case None    => 
          val dir = new Dir(Some(this), path, name)
          subdirs.add(dir)
          dir
      }
    }

    def tryGetSubDir(name: String): Option[Dir] = {
      return subdirs.find(_.name == name)
    }

    def tryGetFile(name: String): Option[File] = {
      return files.find(_.name == name)
    }

    def getOrCreateFile(name: String, path: Path, modTime: Long) : File = {
      return files.find(_.name == name) match {
        case Some(x) => x
        case None    => 
          val file = new File(this, name, path, modTime)
          files.add(file)
          file
      }
    }

    def removeFromParent() {
      parent match {
        case None         => ()
        case Some(parent) => parent.subdirs.remove(this)
      }
    }

    def getAllFiles() : Iterable[File] = {
      var arr = new ArrayBuffer[File]
      def accumulateFiles(dir: Dir) {
        dir.files.foreach(arr.add)
        dir.subdirs.foreach(accumulateFiles)
      }
      accumulateFiles(this)
      return arr
    }
  }

  //
  // Build a directory tree from the list of existing files, populating the dir/file objects as needed
  //
  private def populate(existingfiles: List[ExistingFile]) {
    dirs.clear()
    rootdir = new Dir(None, rootpath, "")
    for (file <- existingfiles) {
      var splits = file.key.split('/')
      var dir  = rootdir
      var path = rootpath
      for (i <- 0 to splits.length - 2) {
        path = path.resolve(splits(i))
        dir  = dir.getOrCreateSubDir(path, splits(i))
      }
      path = path.resolve(splits.last)
      dir.getOrCreateFile(splits.last, path, file.modTime)
    }
  }

  def isSupportedExtension(path: Path): Boolean = {
    var fnstring = path.getFileName().toString().toLowerCase()
    return extensions.exists(fnstring.endsWith(_))
  }

  def getDir(dirpath: Path): Dir = {
    var dirpathstring = dirpath.toString()
    if (!dirpathstring.startsWith(rootpathstring)) throw new IllegalArgumentException(s"invalid path ${dirpathstring} relative to ${rootpathstring}")

    dirpathstring = dirpathstring.substring(rootpathstring.length).stripPrefix("/")

    val splits = dirpathstring.split(java.io.File.separatorChar)
    var dir  = rootdir
    var path = Paths.get(rootpathstring)
    for (comp <- splits) {
      if (comp != "") {
        path = path.resolve(comp)
        dir  = dir.getOrCreateSubDir(path, comp)
      }
    }
    return dir
  }

  def mkFileKey(path: Path) : String = {
    val s = path.toString()
    if (!s.startsWith(rootpathstring)) throw new IllegalArgumentException(s"invalid filekey ${s} relative to ${rootpathstring}")
    s.substring(rootpathstring.length).stripPrefix("/").replace(java.io.File.separatorChar, '/')
  }

  def mkPath(filekey: String) : Path = {
    return Paths.get(rootpathstring + filekey.replace('/', java.io.File.separatorChar))
  }

  // scan a directory, comparing against existing dirs + generating diffs
  def scan(watcher: WatchService, path: Path, dir: Dir, recurse: Boolean) {

    dir.ensureWatch(watcher)

    val dirset  = new HashSet[Dir]()
    val fileset = new HashSet[File]()

    dir.subdirs.foreach(dirset.add)
    dir.files.foreach(fileset.add)

    for (subpath <- Files.newDirectoryStream(path)) {
      var filename = subpath.getFileName().toString()
      if (Files.isDirectory(subpath)) {
        dir.tryGetSubDir(filename) match {
          case Some(subdir) => 
            dirset.remove(subdir)
            if (recurse) {
              scan(watcher, subpath, subdir, recurse);
            }
          case None         =>
            val subdir = dir.getOrCreateSubDir(subpath, filename)
            scan(watcher, subpath, subdir, recurse);
        }
      } else {
        if (isSupportedExtension(subpath)) {
          val modTime  = subpath.toFile().lastModified()
          dir.tryGetFile(filename) match {
            case Some(file) => 
              if (file.updateModTime(modTime)) {
                queue_mod(file)
              }
              fileset.remove(file)
            case None       => 
              var file = dir.getOrCreateFile(filename, subpath, modTime)
              queue_add(file)
          }
        }
      }
    }

    for (file <- fileset) {
      file.dir.files.remove(file)
      queue_remove(file)
    }

    for (dir <- dirset) {
      dir.removeFromParent()
      for (file <- dir.getAllFiles()) {
        queue_remove(file)
      }
      dir.cancelWatch()
    }
  }

  def watcher_thread() {
    val watcher = FileSystems.getDefault().newWatchService();

    Log.info("[dirstorage] performing initialization scan")
    scan(watcher, rootpath, rootdir, true)
    flush();
    Log.info("[dirstorage] initialization scan complete")

    val pending_paths = new HashSet[Path]()
    var overflow      = false

    while (true) {
      var key : WatchKey = null
      try {
        key = watcher.poll(2, TimeUnit.SECONDS)
      } catch {
        case e:InterruptedException => return
      }

      if (key == null) { // no activity in 2s; flush!
        if (overflow) {
          Log.warning(s"[dirstorage] performing scan of ${rootpath} due to overflow")
          scan(watcher, rootpath, rootdir, true)
          flush();
        } else {
          for (dirpath <- pending_paths) {
              Log.trace(s"[dirstorage] scanning ${dirpath}")
              var dir     = getDir(dirpath)
              scan(watcher, dirpath, dir, false)
          }
          flush();
        }
        pending_paths.clear()
        overflow = false
      } else {
        val events = key.pollEvents()
        if (events.exists(_.kind() == OVERFLOW)) {
          overflow = true
        } else {
          for (event <- events) {
            val dirpath = key.watchable().asInstanceOf[Path]
            pending_paths.add(dirpath)
            //Log.trace(s"[dirstorage] enqueueing scan for ${dirpath}")
          }
        }
        key.reset()
      }
    }
  }
}

