package nasko.avrecorder.api

import java.net.URLEncoder

import akka.actor.{Actor, Props, ActorSystem}
import akka.event.Logging
import akka.io.IO
import akka.routing.RoundRobinPool
import nasko.avrecorder._
import org.joda.time.{DateTimeZone, DateTime}
import spray.can.Http
import spray.http.Uri.Query
import spray.http.{MediaTypes, StatusCodes}
import spray.routing.{HttpService, ExceptionHandler}
import spray.util.LoggingContext

import scala.util.{Try, Failure, Success}

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
            Scheduler.stations.find(_.name==stationStr).get.programa.mkString("\n")
          }
        }
      } ~
/*
      path("picked") {
        parameters('station) { stationStr =>
          val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
          val end = start.plusDays(7)
          complete {
            Scheduler.stations.find(_.name==stationStr).get.pick(start, end).mkString("\n")
          }
        }
      } ~
*/
      path("status") { authenticate(basicUserAuthenticator) { authInfo => { ctx =>
        if (!authInfo.hasPermission("Record")) throw new RuntimeException("Not authorized")
        val query = ctx.request.message.uri.query
        //val params = ctx.request.message.uri.query.toMap
        val station = Try{query.getAll("station")} match { case Success(st) if (st!=Nil) => st case _ => List("HristoBotev") }
        val status  = Try{query.getAll("status" )} match { case Success(st) if (st!=Nil) => st case _ => List("Completed","Picked","Registered","Running","Scheduled") }
        val details = query.get("details").getOrElse("off").equalsIgnoreCase("ON")
        val params = query.toMultiMap
        params.keys.find(_.startsWith("record")).map{ str  =>
          val Array(_, station, startMillisStr) = str.split(" ")
          val startMillis = startMillisStr.toLong
          Station.ledger.synchronized {
            Station.ledger.keys.find { a => a.station.name == station && a.start.getMillis == startMillis }.map { article =>
              if (Station.ledger(article) == Status.Registered) {
                scala.tools.nsc.io.File(utils.config.getString("picks")).appendAll(s"$station $startMillisStr\n")
                if (startMillis < Scheduler.nextRefreshTime.getMillis) article.station.schedule(article)
                else Station.ledger += article -> Status.Picked
              }
            }
          }
        }
        respondWithMediaType(MediaTypes.`text/html`) { complete {
          html.status.render(station, status, params, authInfo, details).toString
      }}.apply(ctx)
      }}} ~
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
      path("eskalator") {
        respondWithMediaType(MediaTypes.`text/html`) {
          complete {
            val url = "http://bnr.bg" + ((utils.loadXML("""http://bnr.bg/horizont/search?q=%D0%B5%D1%81%D0%BA%D0%B0%D0%BB%D0%B0%D1%82%D0%BE%D1%80""") \\ "a").find { it => (it \ "@href").text.startsWith("""/horizont/post""")}.get \ "@href").text
            s"""<html><h2><form>Ескалатор Михалеви</h2><BR/><a href="$url">БНР</a><table  border="1" bordercolor="#000000"><tr><td>ПЕСЕН</td><td>ИЗПЪЛНИТЕЛ</td><td></td></tr>${
              val items = utils.loadXML(url) \\ "div" filter { it => (it \ "@class").text == "row-fluid entryRow"}
              items map {
                case <div>{_}<div>{s1 @ _*}</div>{_}<div>{_}<div>{_}<div>{img @ _*}</div>{_}</div>{_}<div>{_}<div>{_}{play @ _}<h5>{song}</h5>{_}<small>{artist}</small>{_}</div>{_}</div>{_}</div>{_}<div>{_*}</div>{_}</div> =>
                  ( song text,
                    artist text,
                    (utils.loadXML("http://bnr.bg" + (play \ "@href" text)) \\ "input").find(it => (it \ "@value").text.startsWith("http")).map(_ \ "@value" text),
                    img \ "@src" text
                    )
                //case _ => "NO"
              } map { case (song, artist, link, pic) =>
                val linkStr = link.getOrElse("")
                s"""<tr><td><a href="$linkStr">$song</a></td><td>$artist</td><td><img src="$pic"/></td></tr>"""  } mkString
            }</table></form></html>"""
          }
        }} ~
      path("shutdown") { authenticate(basicUserAuthenticator) { authInfo => complete {
        if (!authInfo.hasPermission("Shutdown")) throw new RuntimeException("Not authorized")
        if (!Station.ledger.values.exists(_ == Status.Running)) { System.exit(0) ; "Done" }
        else {
          Scheduler.shutdown = true
          "Recording in  progress. Shutdown pending."
        }
      }}}
      // !!!could be buggy !!!
      //path("refresh") { complete { "Refresh completed." + Scheduler.refreshAll} }
  })

}
