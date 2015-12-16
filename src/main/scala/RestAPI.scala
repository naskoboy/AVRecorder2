package nasko.avrecorder.api

import akka.actor.{Actor, Props, ActorSystem}
import akka.event.Logging
import akka.io.IO
import akka.routing.RoundRobinPool
import nasko.avrecorder.{Station, Article, Scheduler, utils}
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
            Scheduler.stations.find(_.name == station).get.programa.mkString("\n")
          }
        }
      } ~
      path("picked") {
        parameters('station) { station =>
          val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
          val end = start.plusDays(7)
          complete {
            Scheduler.stations.find(_.name == station).get.pick(start, end).mkString("\n")
          }
        }
      } ~
      path("request") {
        parameters('article) { articleStr =>
          val reg1 = """([^ ]*) (\d{1,2}):(\d\d) (\d{1,2}):(\d\d) (.*)""".r
          val reg2 = """([^ ]*) (\d{1,2}) (.*)""".r
          val (station, article) =
          articleStr match {
            case reg1(station,h1,m1,h2,m2,title) =>
              val today = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withTimeAtStartOfDay()
              var start = today.plusMinutes(h1.toInt*60+m1.toInt)
              var end   = today.plusMinutes(h2.toInt*60+m2.toInt)
              if (start.isBeforeNow) { start = start.plusDays(1) ; end = end.plusDays(1) }
              (Scheduler.stations.find(_.name == station), Article(start, end, title, title, None))
            case reg2(station,duration,title) =>
              val now = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
              (Scheduler.stations.find(_.name == station), Article(now, now.plusMinutes(duration.toInt), "Requested", title, None))
          }
          complete { station match{
            case Some(st) => st.schedule(article) ; "Request accepted."
            case None => "Unknown station."
          }}

        }
      }
  })

}
