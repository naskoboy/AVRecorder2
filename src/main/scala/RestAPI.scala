package nasko.avrecorder.api

import java.net.URLEncoder

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

import scala.concurrent.ExecutionContext.Implicits.global
class ApiActor extends Actor with HttpService with Authenticator {
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
      path("pick") { authenticate(basicUserAuthenticator) { authInfo =>
      if (!authInfo.hasPermission("Record")) throw new RuntimeException("Not authorized")
        parameters('station, 'startMillis, 'forwardParams) { (station, startMillisStr, forwardParams) =>
          val startMillis = startMillisStr.toLong
          scala.tools.nsc.io.File(utils.config.getString("picks")).appendAll(s"$station $startMillisStr\n")
          Station.ledger.synchronized {
            val article = Station.ledger.keys.find { a => a.station.name == station && a.start.getMillis == startMillis }.get
            if (startMillis < Scheduler.nextRefreshTime.getMillis) article.station.schedule(article)
            else Station.ledger += article -> Status.Picked
          }
          get { ctx => ctx.redirect(s"/api/status?$forwardParams", StatusCodes.Found) }
        }
      }} ~
      path("shutdown") { authenticate(basicUserAuthenticator) { authInfo => complete {
        if (!authInfo.hasPermission("Shutdown")) throw new RuntimeException("Not authorized")
        if (!Station.ledger.values.exists(_ == Status.Running)) { System.exit(0) ; "Done" }
        else {
          Scheduler.shutdown = true
          "Recording in  progress. Shutdown pending."
        }
      }}} ~
      path("status") { authenticate(basicUserAuthenticator) { authInfo =>
        parameters('station.?, 'format.?, 'details.?) { (stationO,format,details) => {
          val recordPermission = authInfo.hasPermission("Record")
          val list = Station.ledger.synchronized{ Station.ledger.filterKeys(it => (stationO==None || stationO.get==it.station.name) && it.start.isAfter(DateTime.now.minusHours(utils.config.getInt("ledger_retention")))).toSeq.sortWith((a,b) => a._1.compare(b._1) == -1)}
          val current = list.map(_._1).filter(it => it.start.isAfterNow || it.end.isAfterNow).min
          val detailsFlag = details.getOrElse("false").toBoolean
          if (format.getOrElse("text")=="html") { ctx => respondWithMediaType(MediaTypes.`text/html`) {
            complete {
              val params = ctx.request.uri.query.toString
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
                article.title +
                  (if (recordPermission && status==Status.Registered) s"""  <a href="/api/pick?station=${article.station.name}&startMillis=${article.start.getMillis}&forwardParams=${URLEncoder.encode(params,"UTF-8")}"> Record</a>"""
                  else "")
              }</td>${
                if (detailsFlag) s"<td>${article.details.getOrElse("").replace("\n","<br>")}</td>" else ""
              }<td>${
                status
              }</td></tr></b>"""}.mkString(s"""<html><body><p><table border="1"><tr><td>BG time</td><td> ${
                utils.ymdHM_format.print(DateTime.now(DateTimeZone.forID("Europe/Sofia")))
              }</td></tr><tr><td>Last Refresh time</td><td> ${
                utils.ymdHMs_format.print(Scheduler.refreshTime)
              }</td></tr><tr><td>Next Refresh time</td><td> ${
                utils.ymdHMs_format.print(Scheduler.nextRefreshTime)
              }</td></tr><tr><td>Refresh results</td><td> ${
                Scheduler.refreshResults.map{
                  case (station, Success(res)) => s"""<a href="/api/status?station=${station.name}&format=html&details=true">${station.name}</a> => $res"""
                  case (station, Failure(e)) => station.name + " => " + e
                }
              }</td></tr>${
                if (Scheduler.shutdown) "<tr><td>Shutdown PENDING</td></tr>"
              }</table><img src=/images/PoweredBy.jpg><table border="1" bordercolor="#000000" width="100%" cellpadding="5" cellspacing="3"><tr><td>STATION</td><td>START</td><td>END</td><td>TITLE</td>${if (detailsFlag) "<td>DETAILS</td>" else ""}<td>STATUS</td></tr>""","","</table></body></html>")
            }
          }.apply(ctx)} else respondWithMediaType(MediaTypes.`text/plain`) { complete { list.mkString("\n") }}
          }}
      }}
/*
      path("request") {
        parameters('article) { articleStr =>
          val reg1 = """([^ ]*)""".r
          val reg2 = """([^ ]*) (\d{1,2})""".r
          val (station, article) =
          articleStr match {
            case reg1(stationStr,h1,m1,h2,m2) =>
              val today = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withTimeAtStartOfDay()
              var start = today.plusMinutes(h1.toInt*60+m1.toInt)
              var end   = today.plusMinutes(h2.toInt*60+m2.toInt)
              if (start.isBeforeNow) { start = start.plusDays(1) ; end = end.plusDays(1) }
              val station = Scheduler.stations(stationStr)
              (station, Article(station, start, end, "title", "title", None))
            case reg2(stationStr,duration) =>
              val now = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
              val station = Scheduler.stations(stationStr)
              (station, Article(station, now, now.plusMinutes(duration.toInt), "Requested", "title", None))
          }
          station.schedule(article)
          Station.ledger.synchronized{ Station.ledger += article->Status.Scheduled}
          complete { "Request accepted." }
        }
      }
*/
      // !!!could be buggy !!!
      //path("refresh") { complete { "Refresh completed." + Scheduler.refreshAll} }
  })

}
