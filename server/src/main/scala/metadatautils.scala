package tvon.server;

import java.nio.file._
import scala.util.matching.Regex

case class BasicMetadata(
  title:        String,
  episodeTitle: Option[String],
  season:       Option[Int],
  episodes:     List[Int],
  year:         Option[Int],
  part:         Option[Int]
)

object MetadataImplicits {
  implicit class MyIterable[A](i: Iterable[A]) {
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

  implicit class MyString(s: String) {
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

    def remove(m: Regex.Match): String = s.substring(0, m.start) + " - " + s.substring(m.end)

    def cleanSuffix(suffixes: Iterable[String]): String = {
      s.iterateUntilClean(s => suffixes.foldLeft(s)((a,suffix) => a.stripSuffix(suffix)))
    }
    def cleanPrefix(prefixes: Iterable[String]): String = {
      s.iterateUntilClean(s => prefixes.foldLeft(s)((a,prefix) => a.stripPrefix(prefix)))
    }
  }
}


// XXX: handle getting series name from dir when we're obviously looking at a series, but the name of the show is missing :(
object MetadataUtils {
  import MetadataImplicits._
  //
  // Try to extract season/episode numbers from stuff
  //
  lazy val seasonEpisode = List(
    """(?:^|[^\d])(\d+)x(\d+)(?:$|[^\d])""",       // 02x03, 2x3, etc
    """(?:^|[^\d])(\d+)x(\d+)(?:$|[^\d])""",       // 02x03, 2x3, etc
    """s(\d+)e(\d+)""",                            // S04E02, S4E3, S04E02, etc.
    """s(\d+) e(\d+)""",                           // S04 E02, etc.
    """s(\d+)-e(\d+)""",                           // S04-E02, etc.
    """(?:^|[^\d])(\d)(\d\d)(?:$|[^\d])""",        // 102
    """(?:^|[^\d])(0\d)(\d\d)(?:$|[^\d])""",       // 0102
    """season (\d+),? episode (\d+)""",            // season 7 episode 1
    """season (\d+) - episode (\d+)""",            // season 7 episode 1
    """[^\.\d](\d+)\.(\d+)[^\.\d]"""               // 1.2, 1.02
  )
  lazy val bracketedSeasonEpisode = seasonEpisode.map("""\[""" + _ + """\]""")
  lazy val parenSeasonEpisode     = seasonEpisode.map("""\(""" + _ + """\)""")
  lazy val seasonEpisodeRegex     = (bracketedSeasonEpisode ++ parenSeasonEpisode ++ seasonEpisode).map(s => ("(?i)" + s).r)

  //
  // Probably need more patterns here, but having trouble finding more examples of multi-episode conventions
  //
  lazy val seasonMultiEpisode = List(
    """s(\d+)e(\d+) ?e(\d+)""",                          
    """s(\d+) e(\d+) ?e(\d+)""",                          
    """s(\d+)-e(\d+) ?e(\d+)"""                
  )
  lazy val bracketedSeasonMultiEpisode = seasonMultiEpisode.map("""\[""" + _ + """\]""")
  lazy val parenSeasonMultiEpisode     = seasonMultiEpisode.map("""\(""" + _ + """\)""")
  lazy val seasonMultiEpisodeRegex     = (bracketedSeasonMultiEpisode ++ parenSeasonMultiEpisode ++ seasonMultiEpisode).map(s => ("(?i)" + s).r)

  lazy val yearRegex = List(
    """[^\d](\d\d\d\d)\.\d\d\.\d\d(?:$|[^\d])""",
    """[^\d](\d\d\d\d)\.\d\d-\d\d(?:$|[^\d])""",
    """[^\d](\d\d\d\d)-\d\d-\d\d(?:$|[^\d])""",
    """[^\d](\d\d\d\d)(?:$|[^\d])"""
  ).map(s => ("(?i)" + s).r)

  lazy val partRegex = List(
    """[^\w\d]C?D(\d+)$""",
    """\-(\d+)$"""
  ).map(s => ("(?i)" + s).r)

  lazy val sceneBaseWords = List(
    "XviD",
    "FEVER",
    "LOL",
    "VTV",
    "720p",
    "1080p",
    "NoTV",
    "HDTV",
    "FQM",
    "TLA",
    "AC3",
    "ASAP",
    "MEDiEVAL",
    "SAVANNAH",
    "FoV",
    "VFUA",
    "SFM",
    "iNTERNAL",
    "DVDRip",
    "DVDrip",
    "TVRip",
    "BRRip",
    "divx",
    "WS",
    "DSR",
    "dsr",
    "xlg47",
    "xvid",
    "REAL",
    "SCRRip",
    "UNRATED",
    "DvDrip",
    "aXXo",
    "commentary",
    "hdtv",
    "RETAIL",
    "INTERNAL",
    "Extended Version",
    "Unrated Edition",
    "digitaldistractions",
    "DVDSCR",
    "dd",
    "DOCUMENT",
    "REPACK",
    "FEVER",
    "SAMPLE",
    "AC-3",
    "NoSubbed",
    "ANGELiC",
    "RiVER",
    "BiA",
    "PDTV",
    "Unaired",
    "hr",
    "ws",
    "pdtv",
    "PROPER",
    "PREAIR",
    "Season Finale",
    "Unrated Edition"
  )

  lazy val sceneWords = sceneBaseWords.map("[" + _) ++ sceneBaseWords.map("(" + _) ++ sceneBaseWords

  lazy val junkStrings = List( 
    "-",
    " ",
    "."
  )

  private def cleanJunk(s:String):String = s.replace("_", " ")
                                            .replace("\t", " ")
                                            .clean("  ", " ")
                                            .clean("- -", "-")
                                            .clean(". .", ".")
                                            .clean("..", ".")
                                            .clean(".", " ")
                                            .cleanSuffix(junkStrings)
                                            .cleanPrefix(junkStrings)
                                            .clean("  ", " ")
                                            .trim

  def extractBasicMetadata(filename: String): BasicMetadata = {

    // 1. remove extension
    val ext_start = filename.lastIndexOf('.')
    val filename0 = ext_start match { case -1 => filename.trim; case n => filename.substring(0, n).trim }

    // 2. extract part off of the end
    val (filename1,part) = partRegex.tryPick(r => {
      r.findFirstMatchIn(filename0).map(m => (filename0.remove(m), Some(m.group(1).toInt)))
    }) getOrElse (filename0, None)

    // 3. extract season/episode stuff if any, remove from string
    val (filename2,season,episodes) = seasonMultiEpisodeRegex.tryPick(r => {
      r.findFirstMatchIn(filename1).map(m => (filename1.remove(m), Some(m.group(1).toInt), List(m.group(2).toInt, m.group(3).toInt)))
    }) getOrElse (seasonEpisodeRegex.tryPick(r => {
      r.findFirstMatchIn(filename1).map(m => (filename1.remove(m), Some(m.group(1).toInt), List(m.group(2).toInt)))
    }) getOrElse (filename1, None, List[Int]()))

    // 4. remove scene junk
    val filename3 = filename2.iterateUntilClean(s => {
      var index = sceneWords.tryPick(word => s.indexOf(word) match { 
        case -1 => None
        case n  => 
          if (n != 0 && !s(n-1).isLetterOrDigit && (n+word.length == s.length || !s(n+word.length).isLetterOrDigit)) {
            Some(n)
          } else {
            None
          }
        })
      index match {
        case None    => s
        case Some(n) => s.substring(0, n)
      }
    })

    // 5. extract year, if any, remove from string
    val (filename4,year) = yearRegex.tryPick(r => {
      r.findFirstMatchIn(filename3).map(m => (filename3.remove(m), Some(m.group(1).toInt)))
    }) getOrElse (filename3, None)

    // 6. clean up lots of junk punctuation
    val filename5 = cleanJunk(filename4)

    // extract episode title/title if needed
    val (title,episodetitle) = 
      season match {
        case Some(_) => filename5.indexOf(" - ") match {
                          case -1 => (filename5, None)
                          case  n => (cleanJunk(filename5.substring(0, n)), Some(cleanJunk(filename5.substring(n+3))))
                        }
        case _       => (filename5, None)
      }

    /*
    // print for debugging
    val from          = filename0.padTo(100, ' ')
    val ptitle        = title.padTo(60, ' ')
    val pepisodetitle = (episodetitle getOrElse "?").padTo(60, ' ')
    val pseason       = season.map(_.toString) getOrElse "?"
    val pepisode      = "[" + (episodes mkString ", ") + "]"
    val pyear         = year.map(_.toString) getOrElse "?"
    val ppart         = part.map(_.toString) getOrElse "?"
    println(s"$from ==> title: $ptitle episode: $pepisodetitle season: $pseason episode: $pepisode year: $pyear part: $ppart")
    */

    new BasicMetadata(
      title        = title,
      episodeTitle = episodetitle,
      season       = season,
      episodes     = episodes,
      year         = year,
      part         = part
    )
  }
}

/*
object BasicMetadataExtractionTest {
  def main(args: Array[String]) {
    import scala.io._   
    Source.fromFile("/Users/Brian/x.txt").getLines().foreach { line =>
      val filename = Paths.get(line).getFileName().toString()
      val metadata = extractBasicMetadata(filename)
    }
  }
}
*/
