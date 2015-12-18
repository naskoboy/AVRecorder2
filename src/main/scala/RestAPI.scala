package nasko.avrecorder.api

import akka.actor.{Actor, Props, ActorSystem}
import akka.event.Logging
import akka.io.IO
import akka.routing.RoundRobinPool
import nasko.avrecorder._
import org.joda.time.{DateTimeZone, DateTime}
import spray.can.Http
import spray.http.{MediaTypes, StatusCodes}
import spray.routing.{HttpService, ExceptionHandler}
import spray.util.LoggingContext

import scala.util.{Failure, Success}

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
        parameters('station) { stationStr =>
          complete {
            Scheduler.stations(stationStr).programa.mkString("\n")
          }
        }
      } ~
      path("picked") {
        parameters('station) { station =>
          val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
          val end = start.plusDays(7)
          complete {
            Scheduler.stations(station).pick(start, end).mkString("\n")
          }
        }
      } ~
      path("status") {
        parameters('station.?, 'format.?, 'details.?) { (stationO,format,details) => {
          val list = Station.ledger.synchronized{ Station.ledger.filterKeys(it => (stationO==None || stationO.get==it.station.name) && it.start.isAfter(DateTime.now.minusHours(3))).toSeq.sortWith((a,b) => a._1.compare(b._1) == -1)}
          val current = list.map(_._1).filter(it => it.start.isAfterNow || it.end.isAfterNow).min
          val detailsFlag = details.getOrElse("false").toBoolean
          if (format.getOrElse("text")=="html") respondWithMediaType(MediaTypes.`text/html`) {
            complete {
              list.map{ case (article,status) => s"""<tr style="background-color: ${
                status match {
                  case Status.Completed => "LightBlue"
                  case Status.Running => "yellow"
                  case Status.Picked => "aqua"
                  case Status.Scheduled => "orange"
                  case _ => ""
                }
              }"><td>${
                article.station.name}</td><td>${
                utils.ymdHM_format.print(article.start)}</td><td>${
                utils.ymdHM_format.print(article.end)}</td><td>${
                (if (article==current) ">>>   " else "") +
                article.title}</td>${
                if (detailsFlag) s"<td>${article.details.getOrElse("").replace("\n","<br>")}</td>" else ""
              }<td>${
                status
              }</td></tr></b>"""}.mkString(s"""<html><body><p>BG time: ${
                utils.ymdHM_format.print(DateTime.now(DateTimeZone.forID("Europe/Sofia")))
              }, Last Refresh time: ${
                utils.ymdHMs_format.print(Scheduler.refreshTime)
              }, Results: ${
                Scheduler.refreshResults.map{
                  case (station, Success(res)) => station.name + " => " + res
                  case (station, Failure(e)) => station.name + " => " + e
                }
              }<img src=/images/PoweredBy.jpg><table border="1" bordercolor="#000000" width="100%" cellpadding="5" cellspacing="3"><tr><td>STATION</td><td>START</td><td>END</td><td>TITLE</td>${if (detailsFlag) "<td>DETAILS</td>" else ""}<td>STATUS</td></tr>""","","</table></body></html>")
            }
          } else respondWithMediaType(MediaTypes.`text/plain`) { complete { list.mkString("\n") }}
          }}
      } ~
      path("request") {
        parameters('article) { articleStr =>
          val reg1 = """([^ ]*) (\d{1,2}):(\d\d) (\d{1,2}):(\d\d) (.*)""".r
          val reg2 = """([^ ]*) (\d{1,2}) (.*)""".r
          val (station, article) =
          articleStr match {
            case reg1(stationStr,h1,m1,h2,m2,title) =>
              val today = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withTimeAtStartOfDay()
              var start = today.plusMinutes(h1.toInt*60+m1.toInt)
              var end   = today.plusMinutes(h2.toInt*60+m2.toInt)
              if (start.isBeforeNow) { start = start.plusDays(1) ; end = end.plusDays(1) }
              val station = Scheduler.stations(stationStr)
              (station, Article(station, start, end, title, title, None))
            case reg2(stationStr,duration,title) =>
              val now = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
              val station = Scheduler.stations(stationStr)
              (station, Article(station, now, now.plusMinutes(duration.toInt), "Requested", title, None))
          }
          station.schedule(article)
          Station.ledger.synchronized(Station.ledger.synchronized{ Station.ledger += article->Status.Scheduled})
          complete { "Request accepted." }
        }
      }

      // !!!could be buggy !!!
      //path("refresh") { complete { "Refresh completed." + Scheduler.refreshAll} }
  })

}
