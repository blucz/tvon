package tvon.server;

import java.nio.file._
import scala.util.matching.Regex
import Extensions._

case class BasicMetadata(
  title:        String,
  episodeTitle: Option[String],
  season:       Option[Int],
  episodes:     List[Int],
  year:         Option[Int],
  part:         Option[Int]
)

// XXX: handle getting series name from dir when we're obviously looking at a series, but the name of the show is missing :(
object MetadataUtils {
  //
  // Try to extract season/episode numbers from stuff
  //
  lazy val seasonEpisode = List(
    """(?:^|[^\d])(\d+)x(\d+)(?:$|[^\d])""",       // 02x03, 2x3, etc
    """(?:^|[^\d])(\d+)x(\d+)(?:$|[^\d])""",       // 02x03, 2x3, etc
    """s(\d+)e(\d+)""",                            // S04E02, S4E3, S04E02, etc.
    """s(\d+) e(\d+)""",                           // S04 E02, etc.
    """s(\d+)-e(\d+)""",                           // S04-E02, etc.
    """s(\d+)\.e(\d+)""",                          // S04.E02, etc.
    """(?:^|[^\d])(\d)(\d\d)(?:$|[^\d])""",        // 102
    """(?:^|[^\d])(0\d)(\d\d)(?:$|[^\d])""",       // 0102
    """season (\d+),? episode (\d+)""",            // season 7 episode 1
    """season (\d+) - episode (\d+)""",            // season 7 episode 1
    """[^\.\d](\d+)\.(\d+)[^\.\d]"""               // 1.2, 1.02
  )
  lazy val bracketedSeasonEpisode = seasonEpisode.map("""\[""" + _ + """\]""")
  lazy val parenSeasonEpisode     = seasonEpisode.map("""\(""" + _ + """\)""")
  lazy val seasonEpisodeRegex     = (bracketedSeasonEpisode ++ parenSeasonEpisode ++ seasonEpisode).map(s => ("(?i)" + s).r)

  lazy val seasonMultiEpisode = List(
    """s(\d+)e(\d+) ?e(\d+)""",                          
    """s(\d+) e(\d+) ?e(\d+)""",                          
    """s(\d+)-e(\d+) ?e(\d+)"""                
  )
  lazy val bracketedSeasonMultiEpisode = seasonMultiEpisode.map("""\[""" + _ + """\]""")
  lazy val parenSeasonMultiEpisode     = seasonMultiEpisode.map("""\(""" + _ + """\)""")
  lazy val seasonMultiEpisodeRegex     = (bracketedSeasonMultiEpisode ++ parenSeasonMultiEpisode ++ seasonMultiEpisode).map(s => ("(?i)" + s).r)

  lazy val episodeNoSeason = List(
    """^(\d\d)[^\d]"""
  )
  lazy val episodeNoSeasonRegex     = episodeNoSeason.map(s => ("(?i)" + s).r)

  lazy val yearRegex = List(
    """(?:^|[^\d])(\d\d\d\d)\.\d\d\.\d\d(?:$|[^\d])""",
    """(?:^|[^\d])(\d\d\d\d)\.\d\d-\d\d(?:$|[^\d])""",
    """(?:^|[^\d])(\d\d\d\d)-\d\d-\d\d(?:$|[^\d])""",
    """(?:^|[^\d])(\d\d\d\d)(?:$|[^\d])"""
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
    "ABSOLUTE",
    "AAC",
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
    "Unrated Edition",
    "Unrated.Edition",
    "UNCUT",
    "BluRay",
    "IVSWESUB",
    "SWESUB",
    "HDTVRip",
    "H264",
    "x264",
    "BRRIP",
    "IIIDVDRiP"
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
                                            .replace("-", " - ")
                                            .clean(". .", ".")
                                            .clean("..", ".")
                                            .clean(".", " ")
                                            .cleanSuffix(junkStrings)
                                            .cleanPrefix(junkStrings)
                                            .clean("  ", " ")
                                            .trim
  private val show_title_sep = "<<<~>>>"

  def extractBasicMetadata(path: Path): BasicMetadata = {
    val filename  = path.getFileName().toString()
    val (parentdir,grandparentdir) = path.getParent() match { 
      case null   => ("","")
      case parent => (parent.getFileName().toString(), parent.getParent() match {
        case null        => ""
        case grandparent => grandparent.getFileName().toString()    
      })
    }
    extractBasicMetadata(filename, parentdir, grandparentdir)
  }

  def extractBasicMetadata(filename: String): BasicMetadata = extractBasicMetadata(filename, "", "")

  def extractBasicMetadata(filename: String, parentdir: String, grandparentdir: String): BasicMetadata = {

    // 1. remove extension
    val ext_start = filename.lastIndexOf('.')
    val filename0 = ext_start match { case -1 => filename.trim; case n => filename.substring(0, n).trim }

    // 2. extract part off of the end
    val (filename1,part) = partRegex.tryPick(r => {
      r.findFirstMatchIn(filename0).map(m => (filename0.remove(m, " "), Some(m.group(1).toInt)))
    }) getOrElse (filename0, None)

    // 3. remove scene junk
    val filename2 = filename1.iterateUntilClean(s => {
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

    // 3. extract season/episode stuff if any, remove from string
    val (filename3,season,episodes) = seasonMultiEpisodeRegex.tryPick(r => {
      r.findFirstMatchIn(filename2).map(m => (filename2.remove(m, show_title_sep), Some(m.group(1).toInt), List(m.group(2).toInt, m.group(3).toInt)))
    }) getOrElse (seasonEpisodeRegex.tryPick(r => {
      r.findFirstMatchIn(filename2).map(m => (filename2.remove(m, show_title_sep), Some(m.group(1).toInt), List(m.group(2).toInt)))
    }) getOrElse (episodeNoSeasonRegex.tryPick(r => {
      r.findFirstMatchIn(filename2).map(m => (filename2.remove(m, show_title_sep), None, List(m.group(1).toInt)))
    }) getOrElse (filename2, None, List[Int]())))

    // 5. extract year, if any, remove from string
    val (filename4,year) = yearRegex.tryPick(r => {
      r.findFirstMatchIn(filename3).map(m => (filename3.remove(m, " "), Some(m.group(1).toInt)))
    }) getOrElse (filename3, None)

    // 6. clean up lots of junk punctuation
    val filename5 = cleanJunk(filename4)

    // extract episode title/title if needed
    val (maybetitle,maybeepisodetitle) = 
      episodes match {
        case Nil     => (filename5, None)
        case _       => filename5.indexOf(show_title_sep) match {
                          case -1 => (filename5, None)
                          case  n => (cleanJunk(filename5.substring(0, n)), Some(cleanJunk(filename5.substring(n+show_title_sep.length))))
                        }
      }

    val episodetitle = maybeepisodetitle match {
      case Some("") => None
      case Some(et) => Some(et)
      case None     => None
    }

    val title = maybetitle match {
      case ""      => (season,episodes) match {
        case (None,Nil) => 
          "Unknown Movie"
        case (None,_)   => 
          parentdir
        case (Some(_),_::_) if parentdir.toLowerCase.contains("season") && !grandparentdir.isEmpty =>
            grandparentdir
        case _ => 
          "Unknown Show"
      }
      case title   => title
    }

    val defaultedseason = if (!episodes.isEmpty && season == None) { Some(1) } else { season }

    new BasicMetadata(
      title        = title,
      episodeTitle = episodetitle,
      season       = defaultedseason,
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
    Source.fromFile("test.txt").getLines().foreach { line =>
      val metadata      = MetadataUtils.extractBasicMetadata(Paths.get(line))
      val from          = line.padTo(100, ' ')
      val ptitle        = metadata.title.padTo(60, ' ')
      val pepisodetitle = (metadata.episodeTitle getOrElse "?").padTo(60, ' ')
      val pseason       = metadata.season.map(_.toString) getOrElse "?"
      val pepisode      = "[" + (metadata.episodes mkString ", ") + "]"
      val pyear         = metadata.year.map(_.toString) getOrElse "?"
      val ppart         = metadata.part.map(_.toString) getOrElse "?"
      println(s"$from ==> title: $ptitle episode: $pepisodetitle season: $pseason episode: $pepisode year: $pyear part: $ppart")
    }
  }
}
*/
