package tvon.server

import org.scalatra._

class ApiServlet(manager:Manager) extends ScalatraServlet {
  get("/") {
    "Hello, World"
  }
}

class ApiServer(manager: Manager, port: Int) extends ScalatraServlet {
    import org.eclipse.jetty.server._
    import org.eclipse.jetty.servlet._
    import org.eclipse.jetty.server.handler._

    val server  = new Server(port)
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    val servlet = new ApiServlet(manager)
    context.setContextPath("/")
    context.addServlet(new ServletHolder(servlet), "/*")
    server.setHandler(context)

    def start() {
        server.start()
    }

    def stop() {
      server.stop()
      server.join()
    }
}

