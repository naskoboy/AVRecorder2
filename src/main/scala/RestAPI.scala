package nasko.avrecorder.api

import akka.actor.{Actor, Props, ActorSystem}
import akka.event.Logging
import akka.io.IO
import akka.routing.RoundRobinPool
import nasko.avrecorder.Scheduler.HristoBotev
import nasko.avrecorder.{Scheduler, utils}
import org.joda.time.{DateTimeZone, DateTime}
import spray.can.Http
import spray.http.StatusCodes
import spray.routing.{HttpService, ExceptionHandler}
import spray.util.LoggingContext

/**
 * Created by nasko on 12/15/2015.
 */
object ApiBoot extends App {

  implicit val system = ActorSystem("spray-api-service")
  val log = Logging(system, getClass)

  // create and start our service actor
  val service = system.actorOf(RoundRobinPool(utils.config.getInt("concurrentActors")).props(Props[ApiActor]), "router")

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, interface = "0.0.0.0", port = utils.config.getInt("port"))


}


class ApiActor extends Actor with HttpService {

  implicit def myExceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e: Exception =>
        ctx => {
          log.error(e.getMessage)
          ctx.complete(StatusCodes.BadRequest, e.getMessage)
        }
    }

  def actorRefFactory = context
  def receive = runRoute(
    pathPrefix("api") {
      path("programa") {
        parameters('station) { station =>
          complete {
            Scheduler.stations.find(_.name==station).get.programa.mkString("\n")
          }
        }
      } ~
      path("picked") {
        val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
        val end = start.plusDays(7)
        complete {
          HristoBotev.pick(start, end).mkString("\n")
        }
      } ~
    pathPrefix("api") {
      path("jsonConnector") {
        getFromResource("jsonConnector.html")
      }

    }
  })
}
