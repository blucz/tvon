package tvon.server

import java.io.FileNotFoundException
import scala.io._   
import org.json4s._
import org.json4s.native.JsonMethods._

case class DirectoryConfig(id: String, name: String, path: String)
case class Config(
  datapath:    String,
  webroot:     String,
  port:        Int, 
  directories: List[DirectoryConfig]
)

object Config {
    def load(path : String) : Either[Config,String] = {
        implicit val formats = DefaultFormats
        try { 
          val text   = Source.fromFile(path).mkString
          val ast    = parse(StringInput(text))
          val config = ast.extract[Config]
          Left(config)
        } catch {
          case e:FileNotFoundException => Right(s"config file ${path} not found")
          case e:Exception             => Right(s"error parsing ${path}: ${e.getMessage()}")
        }
    }
}
