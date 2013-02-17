package tvon.server

import org.scalatra._
import java.nio.file._
import java.io.File

class WebServlet(manager: Manager, webroot: String) extends ScalatraServlet {
  import ApiHelpers._

  val collection = manager.collection
  val profiles   = manager.profiles

  val webrootpath = Paths.get(webroot)

  def setJson() {
      response.setHeader("Content-Type", "application/json")
  }

  get("/") {
    redirect("/webui/index.html")
  }

  //
  // Videos API
  //
  get("/api/videos") {
    setJson()
    new ApiVideoList(collection.videos.values.map(_.toApi).toList).toResponseJson
  }

  get("/api/videos/:id") {
    setJson()
    collection.videos.get(params("id")) match {
      case None        => errorResponse("notfound")
      case Some(video) => video.toApi.toResponseJson
    }
  }

  //
  // Profiles API
  //
  get("/api/profiles") {
    setJson()
    new ApiProfileList(profiles.profiles.values.map(_.toApi).toList).toResponseJson
  }

  get("/api/profiles/:id") {
    setJson()
    profiles.profiles.get(params("id")) match {
      case None          => errorResponse("notfound")
      case Some(profile) => profile.toApi.toResponseJson
    }
  }

  post("/api/profiles/create") {
    setJson()
    params.get("name") match {
      case Some(name) => 
        if (profiles.profiles.values.exists(_.name == name)) {
          errorResponse("duplicate")
        } else {
          val profile = profiles.create(name)
          profile.toApi.toResponseJson
        }
      case _          => errorResponse("missingparams")
    }
  }

  post("/api/profiles/delete") {
    setJson()
    params.get("id") match {
      case Some(id) =>
        profiles.profiles.get(id) match {
          case Some(profile) => profiles.delete(profile)
                                emptyResponse()
          case _             => errorResponse("notfound")
        }
      case _ => 
        errorResponse("missingparams")
    }
  }

  post("/api/profiles/edit") {
    setJson()
    (params.get("name"),params.get("id")) match {
      case (Some(name),Some(id)) => 
        profiles.profiles.get(id) match {
          case Some(profile) => 
            if (profiles.profiles.values.exists(p => p.name == name && p.profileId != id)) {
              errorResponse("duplicate")
            } else {
              profiles.edit(profile, name)
              profile.toApi.toResponseJson
            }
          case None => 
            errorResponse("notfound")
        }
        if (profiles.profiles.values.exists(_.name == name)) {
          errorResponse("duplicate")
        } else {
          val profile = profiles.create(name)
          profile.toApi.toResponseJson
        }
      case _ => 
        errorResponse("missingparams")
    }
  }

  //
  // Handle static files for web ui
  //
  get ("/favicon.ico") { redirect("/webui/favicon.ico") }

  get("/webui/*") {
    val reqpath  = multiParams("splat").head.replace('/', File.separatorChar)
    val filepath = webrootpath.resolve(reqpath)

    if (reqpath.contains("..")) {
      response.setStatus(403)
      "Forbidden"
    } else if (!Files.exists(filepath)) {
      response.setStatus(404)
      "Not found"
    } else {
      response.setHeader("Content-Type", filepath.getFileName.toString match {
        case p if p.endsWith(".html") => "text/html"
        case p if p.endsWith(".css")  => "text/css"
        case p if p.endsWith(".js")   => "application/javascript"
        case p if p.endsWith(".js")   => "application/javascript"
        case p if p.endsWith(".jpg")  => "image/jpeg"
        case p if p.endsWith(".png")  => "image/png"
        case p if p.endsWith(".ico")  => "application/x-icon"
        case _                        => "application/octet-stream"
      })
      filepath.toFile
    }
  }

  notFound {
    response.setStatus(404)
    "Not found"
  }
}

class WebServer(manager: Manager, webroot: String, port: Int) extends ScalatraServlet {
    import org.eclipse.jetty.server._
    import org.eclipse.jetty.servlet._
    import org.eclipse.jetty.server.handler._

    val server  = new Server(port)
    val servlet = new WebServlet(manager, webroot)

    val servlethandler = new ServletContextHandler(ServletContextHandler.SESSIONS)
    servlethandler.setContextPath("/")
    servlethandler.addServlet(new ServletHolder(servlet), "/*")
    server.setHandler(servlethandler)

    def start() {
        server.start()
    }

    def stop() {
      server.stop()
      server.join()
    }
}

//
// Helper methods to make API code cleaner
//
object ApiHelpers {
  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.native.Serialization

  implicit class RichA[A <: AnyRef](r: A) (implicit m: Manifest[A]) {
    implicit val formats = DefaultFormats
    def toJson         : String         = Serialization.write(r)
    def toResponse     : ApiResponse[A] = new ApiResponse("success", r)
    def toResponseJson : String         = toResponse.toJson
  }

  case class ApiEmpty()
  lazy val empty = new ApiEmpty()
  def errorResponse(s: String) = new ApiResponse(s,         empty).toJson
  def emptyResponse()          = new ApiResponse("success", empty).toJson
}

//
// Represents a response (status + optional value)
//
case class ApiResponse[A <: AnyRef](status: String, value: A)(implicit m: Manifest[A])

