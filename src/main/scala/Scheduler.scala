package nasko.avrecorder

import java.io.{PrintWriter, File}
import java.lang.Throwable
import java.util.Properties
import java.util.concurrent.{TimeUnit, Executors}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.{Logger, LazyLogging}
import nasko.avrecorder.api.ApiBoot
import org.joda.time.format.DateTimeFormat
import org.joda.time._
import org.slf4j.LoggerFactory
import scala.collection.mutable.StringBuilder
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.xml.Node

/**
 * Created by nasko on 11/20/2015.
 */

case class Reader[C, A](g: C => A) {
  def apply(c: C) = g(c)
  def map[B](f: A => B): Reader[C, B] = flatMap(a => Reader(c => f(a)) )  // Reader(c => f(g(c)))
  def flatMap[B](f: A => Reader[C, B]): Reader[C, B] = Reader(c => f(g(c))(c))
}

object eskalatorGenerator extends App {
  val url = "http://bnr.bg" + ((utils.loadXML("""http://bnr.bg/horizont/search?q=%D0%B5%D1%81%D0%BA%D0%B0%D0%BB%D0%B0%D1%82%D0%BE%D1%80""") \\ "a").find { it => (it \ "@href").text.startsWith("""/horizont/post""")}.get \ "@href").text
  val items = utils.loadXML(url) \\ "div" filter { it => (it \ "@class").text == "row-fluid entryRow"}
  val items2 = items map {
    case <div>{_}<div>{s1 @ _*}</div>{_}<div>{_}<div>{_}<div>{img @ _*}</div>{_}</div>{_}<div>{_}<div>{_}{play @ _}<h5>{song}</h5>{_}<small>{artist}</small>{_}</div>{_}</div>{_}</div>{_}<div>{_*}</div>{_}</div> =>
      ( song text,
        artist text,
        (utils.loadXML("http://bnr.bg" + (play \ "@href" text)) \\ "input").find(it => (it \ "@value").text.startsWith("http")).map(_ \ "@value" text),
        img \ "@src" text
        )
    //case _ => "NO"
  }
  val html = s"""<html><form><h2>Ескалатор Михалеви<a href="$url">БНР</a></h2><table  border="0" bordercolor="#000000">${
    items2 map { case (song, artist, link, pic) =>
      val linkStr = link.getOrElse("")
      s"""<tr><td><h2><a href="$linkStr">$song</a>$artist</h2></td></tr><tr><td><img src="$pic" width="700" height="350" /></td></tr>"""  } mkString
  }</table></form></html>"""
  val filename = utils.config.getString("home") + """\eskalator.html"""
  scala.tools.nsc.io.File(filename).writeAll(html)
  utils.logger.info(s"$filename generated.")
}

object utils extends LazyLogging  {

  def flvAudioViaRtmp(url: String)(article: Article) = {
    val fullFileName = article.getFullFilename + ".flv"
    val cmd = s"""${utils.config.getString("rtmpdump")} -r $url -o $fullFileName"""
    logger.info(cmd)
    import sys.process._
    val process = cmd.run
    // can't capture rtmp-errors
    Thread.sleep(1000*(Seconds.secondsBetween(DateTime.now, article.end).getSeconds+article.station.rightPadding*60))
    process.destroy()
    logger.info(s"$fullFileName COMPLETED")
    fullFileName
  }

  def mp3AudioViaRtmpVlc(url: String)(article: Article) = Try {
    val fullFileName = article.getFullFilename + ".mp3"
    val duration = Seconds.secondsBetween(DateTime.now, article.end).getSeconds+article.station.rightPadding*60
    val rtmp = s""""${utils.config.getString("rtmpdump")}" -r $url"""
    val vlc = s""""${config.getString("vlc")}" - --sout #transcode{acodec=mp3,vcodec=dummy,ab=128,channels=2,samplerate=44100}:std{dst="$fullFileName",access=file} --run-time=$duration -I dummy --dummy-quiet vlc://quit"""
    logger.info(rtmp + " | " + vlc)
    import sys.process._
    val sb = new StringBuilder
    var error = false
    val process = (rtmp #> vlc).run(ProcessLogger{ line =>
      //println(line)
      if (line != "ERROR: Download: Failed writing, exiting!" && line.startsWith("ERROR")) { error = true ; sb.append(">>> ") }
      if (!line.endsWith("sec")) sb.append(line + "\n") // ignore "? kb / ? sec" msgs from rtmp
    })
    //Thread.sleep(1000*(Seconds.secondsBetween(DateTime.now, article.end).getSeconds+article.station.rightPadding*60))
    //process.destroy()
    process.exitValue()
    if (error) throw new RuntimeException(sb.mkString)
    logger.info(s"$fullFileName COMPLETED")
    fullFileName
  }

  def fixMp3Tags(filename: String, article: Article) = {
    logger.info(s"$filename fixing mp3 tags ...")
    // fix mp3 tag, https://github.com/soc/jaudiotagger
    import org.jaudiotagger.audio.AudioFileIO
    import org.jaudiotagger.tag.{FieldKey, TagOptionSingleton}
    import org.jaudiotagger.tag.id3.valuepair.TextEncoding
    TagOptionSingleton.getInstance.setId3v23DefaultTextEncoding(TextEncoding.getInstanceOf.getIdForValue("UTF-8").byteValue())

    val tag = new org.jaudiotagger.tag.id3.ID3v23Tag()
    tag.addField(FieldKey.ALBUM, article.minimalTitle)
    article.details match {
      case Some(details) =>
        if (details.size<=50) { tag.addField(FieldKey.TITLE, details) }
        else {
          tag.addField(FieldKey.TITLE, article.title)
          tag.addField(FieldKey.COMMENT, details)
        }
      case None =>
        tag.addField(FieldKey.TITLE, article.title)
    }
    val file = AudioFileIO.read(new File(filename))
    file.setTag(tag)
    file.commit
    logger.info(s"$filename mp3 tags Fixed")
    filename
  }

  def bnrFactory(url: String): Reader[Article,Try[String]] = Reader((article: Article) => {
    Station.ledger.synchronized{ Station.ledger += article->Status.Running }
    try utils.mp3AudioViaRtmpVlc(url)(article) map (fn => utils.fixMp3Tags(fn, article))
    finally {
      Station.ledger.synchronized { Station.ledger += article -> Status.Completed }
    }
  })

  def rtmpVideo(params: Map[String,String])(article: Article): Try[String] = {
    Station.ledger.synchronized{ Station.ledger += article->Status.Running }
    val fullFileName = article.getFullFilename + "_" + utils.getFixedString(article.details match { case Some(d) => d case _ => ""})  + ".flv"
    val fullFileNameTmp = fullFileName + ".tmp"
    import sys.process._
    val cmd = s""""${config.getString("rtmpdump")}" -v --quiet --stop 14400 --timeout 240 -o "$fullFileNameTmp" ${params.map(it => s"${it._1} ${it._2}").mkString(" ")}"""
    logger.info(cmd)
    val process = cmd.run
    // can't capture rtmp-errors
    Thread.sleep(1000*(Seconds.secondsBetween(DateTime.now, article.end).getSeconds+article.station.rightPadding*60))
    process.destroy()
    // http://yamdi.sourceforge.net/
    s""""${config.getString("yamdi")}" -i "$fullFileNameTmp" -o "$fullFileName" -w""".run.exitValue() // wait until completes
    if (!new File(fullFileNameTmp).delete()) logger.warn(s"Failed to delete $fullFileNameTmp")
    logger.info(s"$fullFileName COMPLETED")
    Station.ledger.synchronized{ Station.ledger += article->Status.Completed}
    Success(fullFileName)
  }

  override lazy val logger = Logger(LoggerFactory.getLogger(""))
  val scheduler = Executors.newScheduledThreadPool(5)
  val time_reg = ".т (\\d{1,2}).(\\d\\d) д. (\\d{1,2}).(\\d\\d) часа".r
  val day_reg = """(.*),(?: *)(\d{1,2}) (.*)""".r
  val day_reg_long = """(\d{1,2}) (.*) (\d\d\d\d), (.*)""".r
  val time_hh_mm = """(\d\d):(\d\d)""".r
  val timeslot_reg = """(\d\d):(\d\d) (\d\d):(\d\d) (.*)""".r

  // https://www.branah.com/unicode-converter
  val months = Vector("януари","февруари","март","април","май","юни","юли","август","септември","октомври","ноември","декември")
  val published_reg = "публикувано на (\\d\\d).(\\d\\d).(\\d\\d)(.*)".r
  val ymdHM_format = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  val ymdHMs_format = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  val HM_format = DateTimeFormat.forPattern("HH:mm")

  val config = ConfigFactory.load()

  val bgAlphabet = " 0123456789АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЪЮЯабвгдежзийклмнопрстуфхцчшщьъюя"
  val engAlphabet = Array(
    "_", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "A", "B", "V", "G", "D", "E", "J", "Z", "I", "J", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F", "H", "C", "CH", "SH", "ST", "A", "A", "IU", "IA",
    "a", "b", "v", "g", "d", "e", "j", "z", "i", "j", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "c", "ch", "sh", "st", "a", "a", "iu", "ia"
  )

  def getFixedString(s: String) = s.zip("a"+s).map{ case (a:Char,b:Char) =>
    val index = bgAlphabet.indexOf(a)
    val r:String =
      if (index>=0) engAlphabet(index) else
      if (('a' <= a && a <= 'z') || ('A' <= a && a <= 'Z')) a.toString
      else "_"
    if (b==' ') r.toUpperCase else r
  }.mkString

  def loadXML(url: String) = {
    val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
    val parser = parserFactory.newSAXParser
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    val source = new org.xml.sax.InputSource(url)
    adapter.loadXML(source, parser)
  }
/*
  def executeCommand(cmd: String, mystdout: StringBuilder, mystderr: StringBuilder) = {
    import sys.process._
    Process(cmd) ! ProcessLogger(it => { mystdout append (it+"\n") }, it => { mystderr append (it+"\n")})
  }

  val rtmpVideoRecorder = (params: Map[String,String]) => (article: Article) => Try {
    Station.ledger.synchronized{ Station.ledger += article->Status.Running }
    val fullFileName = article.getFullFilename + "_" + utils.getFixedString(article.details match { case Some(d) => d case _ => ""})  + ".flv"
    val fullFileNameTmp = fullFileName + ".tmp"
    import sys.process._
    val cmd = s""""${config.getString("rtmpdump")}" -v --quiet --stop 14400 --timeout 240 -o "$fullFileNameTmp" ${params.map(it => s"${it._1} ${it._2}").mkString(" ")}"""
    logger.info(cmd)
    val process = cmd.run
    // can't capture rtmp-errors
    Thread.sleep(1000*(Seconds.secondsBetween(DateTime.now, article.end).getSeconds+article.station.rightPadding*60))
    process.destroy()
    // http://yamdi.sourceforge.net/
    s""""${config.getString("yamdi")}" -i "$fullFileNameTmp" -o "$fullFileName" -w""".run.exitValue() // wait until completes
    if (!new File(fullFileNameTmp).delete()) logger.warn(s"Failed to delete $fullFileNameTmp")
    logger.info(s"$fullFileName COMPLETED")
    Station.ledger.synchronized{ Station.ledger += article->Status.Completed}
    ()
  }
*/
  def adjustArticles(articles: Seq[Article]) = {
    val list = articles.foldLeft((List.empty[Article], DateTime.now.minusYears(1))){ (acc, it) =>
      if (it.start.isAfter(acc._2)) (it :: acc._1, it.start)
      else (it.copy(start = it.start.plusDays(1)) :: acc._1, it.start.plusDays(1))
    }._1.reverse
    (list.zip(list.tail)).map{ case (a1,a2) => a1.copy(end = a2.start) }
  }

  def sendEmail(from: String, to: Seq[String], subject: String, text: String) {
    import javax.mail.Message
    import javax.mail.internet.{InternetAddress, MimeMessage}

    // Setup mail server
    val props = new Properties()
    props.put("mail.smtp.host", "smtp")
    val session = javax.mail.Session.getDefaultInstance(props, null)

    // Define message
    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(from))
    to foreach { r => message.addRecipient(Message.RecipientType.TO, new InternetAddress(r)) }
    message.setSubject(subject)
    message.setText(text)

    // Send message
    javax.mail.Transport.send(message)
  }

}

case class Article(station: Station, start: DateTime, end: DateTime, minimalTitle: String, title: String, details: Option[String]) extends Ordered[Article] {

  def compare (that: Article) = {
    if (this.station.name<that.station.name) -1 else
    if (this.station.name>that.station.name) +1 else
    if (this.start.isBefore(that.start)) -1 else
    if (this.start.isAfter(that.start)) +1
    else 0
  }

  override def toString() = s"${ utils.ymdHM_format.print(start) } >> ${
    if (start.isBefore(end)) utils.HM_format.print(end) else "???"
  }  $title [${
    details.getOrElse("")
  }]"

  def correspondence(that: Article) = {
    val t1 = if (this.start.isAfter (that.start)) this.start else that.start
    val t2 = if (this.end  .isBefore(that.end  )) this.end   else that.end
    if (t1.isAfter(t2)) .0
    else Minutes.minutesBetween(t1,t2).getMinutes.toDouble/Minutes.minutesBetween(this.start,this.end).getMinutes
  }

  def correspondenceArticle(items: Seq[Article]) =  items.foldLeft((0.0, None: Option[Article])) { (acc, it) =>
    val corr = this.correspondence(it)
    if (corr > acc._1) (corr, Some(it)) else acc
  }._2

  private val timestamp_format = DateTimeFormat.forPattern("yyMMdd_HHmm")
  def getFullFilename = s"""${station.destination}\\${station.prefix}_${utils.getFixedString(title)}_${timestamp_format.print(start)}"""

}

object Status extends Enumeration {
  //type Status = Value
  val Registered,Picked,Scheduled,Running,Completed = Value
}

object Station {
  val ledger = collection.mutable.Map.empty[Article,Status.Value]
}
abstract class Station(
  val name: String,
  val prefix: String,
  //val url: String,
  val destination: String,
  val leftPadding: Int,
  val rightPadding: Int
) {

  val factory: Reader[Article,Try[String]] = Reader(a => Success(""))

  def programa: Seq[Article]

  def getSubscriptions = {
    val picker_reg = """([^ ]*) (.*)""".r
    // """([^ ]*) ([^,]*)(.*)$?"""
    val timeslot_reg = """(.*) TimeSlot (\d) (\d\d):(\d\d) (\d\d):(\d\d) (.*)""".r
    val subscriptions = scala.io.Source.fromFile(utils.config.getString("subscriptions")).getLines().toList.filter(it => it != "" && it(0)!="#")
    subscriptions.foldLeft(List.empty[String], List.empty[Article]){ (acc,it) => it match {
      case timeslot_reg(station,day,h1,m1,h2,m2,title) if (station == name) =>
        val now = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withMillisOfDay(0)
        val nextDay = now.plusDays((day.toInt-now.dayOfWeek().get()+7)%7)
        (acc._1, Article(this,nextDay.withHourOfDay(h1.toInt).withMinuteOfHour(m1.toInt), nextDay.withHourOfDay(h2.toInt).withMinuteOfHour(m2.toInt), title, title, None) :: acc._2)
      case picker_reg(station,words) if (station == name) =>
        (words :: acc._1, acc._2)
      case _ => (acc._1, acc._2)
    }}
  }

  def schedule(article: Article): Unit = {
    utils.scheduler.schedule(new Runnable{ def run() = {
      val res = article.station.factory.apply(article)
      if (Scheduler.shutdown && !Station.ledger.values.exists(_ == Status.Running)) System.exit(0)
      res
    }}, article.start.getMillis - System.currentTimeMillis - article.station.leftPadding*60*1000L, TimeUnit.MILLISECONDS)
    Station.ledger.synchronized{ Station.ledger += article->Status.Scheduled}
    utils.logger.info(s"scheduled $name $article")
  }

  def refresh(start: DateTime, end: DateTime, picks: List[(String,Long)]) = Try {

    def coming(a: Article) = a.start.isAfter(start) || a.end.isAfter(start)

    val prog = programa.filter(coming)
    if (!prog.isEmpty) {
      val (pickers, timeslots) = getSubscriptions
      Station.ledger.synchronized {
        val toberemoved = Station.ledger.iterator.filter{ case (article,status) =>
          article.station==this &&
            (article.start.isAfter(start) ||
              (article.start.isBefore(start.minusHours(utils.config.getInt("ledger_retention"))) && status!=Status.Running))
        }.map(_._1)
        toberemoved.foreach(Station.ledger.remove)
        (prog ++ timeslots).filter(coming).foreach{ article =>
          if (pickers.exists(pk => article.title.indexOf(pk) >= 0 || (pk(0)=='@' && article.details.toString.indexOf(pk.substring(1)) >= 0)) || (picks.exists(it => article.station.name==it._1 && article.start.getMillis==it._2))) {
            if (article.start.isAfter(start) && article.start.isBefore(end)) schedule(article)
            else Station.ledger += article->Status.Picked
          }
          else Station.ledger += article->Status.Registered
        }
      }}
      "OK"
  }

  def minusWords(a: String, b: String) = { val bb = b.split(" ") ; a.split(" ").filter(it => it.size>3 && !b.exists(_.equals(it))).size }
  def expandedTitle(a: String, b: String) = { val (r1, r2) = (minusWords(a,b), minusWords(b,a)) ; if (r1<r2) b else a }
  def expandedTime(a1: DateTime, a2: DateTime, b1: DateTime, b2: DateTime) = (if (a1.isBefore(b1)) a1 else b1, if (a2.isAfter(b2)) a2 else b2)

  def enrich(weeklyProgram: Seq[Article], izbrano: Seq[Article]) = {
    weeklyProgram.map(it =>
      it.correspondenceArticle(izbrano) match {
        case Some(izb) =>
          val (t1,t2) = expandedTime(it.start,it.end,izb.start,izb.end)
          Article(this,t1,t2,it.title,expandedTitle(it.title,izb.title),izb.details)
        case None => it
      }
    )
  }

}


object Scheduler {

  var refreshTime = DateTime.now
  var nextRefreshTime = DateTime.now
  var refreshResults = Seq.empty[(Station, Try[String])]
  var shutdown = false

  def bnr_sedmichna_programa(station: Station, doc: Node) = {

    def strip(node:Seq[Node]): Seq[Node] = node.flatMap{
      case node:Node if (node.label=="#PCDATA") => Nil
      case <module>{_}<div>{cont @ _*}</div>{_}</module> => strip(cont)
      case it => Seq(it)
    }

    val moduleNode = ((doc \\ "div").apply(_.\@("class")=="news_content") \ "module").head.head
    val cont_flat = strip(moduleNode)
    val locations = cont_flat.foldLeft((List.empty[Int],0))((acc,it) => it match { case <h2>{_}</h2> => (acc._2 :: acc._1, acc._2+1) case _ => (acc._1, acc._2+1) })._1.reverse
    val location_splits = locations.zip(locations.drop(1) ++ List(cont_flat.size))
    val items = location_splits.map(it => cont_flat.drop(it._1).take(it._2-it._1))
    items.flatMap{ case dateNode :: articleNodes => {
      val utils.day_reg_long(dateStr, monthStr, yearStr, dayStr) = dateNode.text
      val datetime = new DateTime(yearStr.toInt, utils.months.indexOf(monthStr)+1, dateStr.toInt, 0, 0, DateTimeZone.forID("Europe/Sofia"))
      val articles = articleNodes.map{ art =>
        val <div>{_}<div>{time}</div>{item @ _*}</div> = art
        val title = (try item(1) catch { case _ => item(0) }).text.trim
        val utils.time_hh_mm(h,m) = time.text
        Article(station,datetime.plusMinutes(h.toInt*60+m.toInt), datetime, title, title, None)
      }
      (articles.zip(articles.tail ++ List(Article(station,datetime.plusDays(1),datetime,"","",None)))).map{ case (a1,a2) =>  a1.copy(end = a2.start) }
    }}
  }

  import utils.config
  object HristoBotev extends Station(
      config.getString("stations.HristoBotev.name"),
      config.getString("stations.HristoBotev.prefix"),
      //config.getString("stations.HristoBotev.url"),
      config.getString("stations.HristoBotev.destination"),
      config.getInt("stations.HristoBotev.leftPadding"),
      config.getInt("stations.HristoBotev.rightPadding")
  ) {

    override val factory: Reader[Article,Try[String]] = utils.bnrFactory(config.getString("stations.HristoBotev.rtmp_url"))

    def programa = {
      val weeklyDoc = utils.loadXML("""http://bnr.bg/hristobotev/page/sedmichna-programa""")
      val weeklyProgram = bnr_sedmichna_programa(this, weeklyDoc)
      val izbrano = ((weeklyDoc \\ "div").find(it => (it \ "@class").text == "row-fluid module_container").get \\ "a").flatMap(it => hb_izbrano("http://bnr.bg" + it \ "@href") match { case Success(l) => l case _ => Nil }).sorted(Ordering[Article])
      enrich(weeklyProgram, izbrano)
    }

    def hb_izbrano(url: String) = Try {

      def strip(node:Seq[Node]): Seq[Node] = node.flatMap{
        case <br/> => Nil
        case <p>{items @ _*}</p>       => strip(items)
        case <span>{items @ _*}</span> => strip(items)
        case <div>{items @ _*}</div>   => strip(items)
        case it => Seq(it)
      }
      val doc = utils.loadXML(url)
      val utils.published_reg(pDateStr,pMonthStr,pYearStr,_) = (doc \\ "span").find(it => (it \ "@itemprop").text == "datePublished").get.text
      val pDate = new DateTime(2000+pYearStr.toInt, pMonthStr.toInt, pDateStr.toInt,0,0,0,0, DateTimeZone.forID("Europe/Sofia"))

      val news_title = (doc \\ "div").find(it => (it \ "@class").text == "news_title").get
      val (dayNode, postedNode, cont) = news_title match { case <div>{_}<h1>{day @ _*}</h1>{_}<div>{date @ _*}</div>{_}<div>{cont @ _*}</div>{w @ _*}</div> => (day, date, cont)}
      val utils.day_reg(dayStr, dateStr, monthStr) = dayNode.text

      val datetime = {
        val candidate = new DateTime(2000+pYearStr.toInt, utils.months.indexOf(monthStr)+1, dateStr.toInt, 0,0,0,0, DateTimeZone.forID("Europe/Sofia"))
        if (candidate.isAfter(pDate)) candidate
        else new DateTime(2000+pYearStr.toInt+1, utils.months.indexOf(monthStr)+1, dateStr.toInt, 0,0,0,0, DateTimeZone.forID("Europe/Sofia"))
      }

      val cont_flat = strip(cont)
      val locations = cont_flat.foldLeft((List.empty[Int],0))((acc,it) => it match { case <b>{_}{some @ _*}</b> => (acc._2-1 :: acc._1, acc._2+1) case _ => (acc._1, acc._2+1) })._1.reverse.drop(1)
      val location_splits = locations.zip(locations.drop(1) ++ List(cont_flat.size))
      val items = location_splits.map(it => cont_flat.drop(it._1).take(it._2-it._1))
      items.flatMap{
        case date :: title :: details =>
          val (t1, t2) = date.text.trim match {
            case utils.time_reg(h1,m1,h2,m2) => (h1.toInt*60+m1.toInt, h2.toInt*60+m2.toInt)
            case _ =>  (0,0)
          }
          List(Article(this, datetime.plusMinutes(t1), datetime.plusMinutes(t2), title.text, title.text, Some(details.map(_.text).mkString("\n"))))
        case _ => List()
      }
    }

  }

  object Horizont extends Station(
    config.getString("stations.Horizont.name"),
    config.getString("stations.Horizont.prefix"),
    //config.getString("stations.Horizont.url"),
    config.getString("stations.Horizont.destination"),
    config.getInt("stations.Horizont.leftPadding"),
    config.getInt("stations.Horizont.rightPadding")
  ) {
    def programa = bnr_sedmichna_programa(this, utils.loadXML("""http://bnr.bg/horizont/page/programna-shema"""))

    override val factory: Reader[Article,Try[String]] = utils.bnrFactory(config.getString("stations.Horizont.rtmp_url"))

  }

  object BntWorld extends Station(
    config.getString("stations.BntWorld.name"),
    config.getString("stations.BntWorld.prefix"),
    //config.getString("stations.BntWorld.url"),
    config.getString("stations.BntWorld.destination"),
    config.getInt("stations.BntWorld.leftPadding"),
    config.getInt("stations.BntWorld.rightPadding")
//    utils.rtmpVideoRecorder(Map("-r" -> "rtmp://193.43.26.198:1935/live/bntsat"))
  ) {

    override val factory: Reader[Article,Try[String]] = Reader(utils.rtmpVideo(Map("-r" -> "rtmp://193.43.26.198:1935/live/bntsat")))

    def bntw_izbrano = Seq[Article]()

    def programa = {
      //val daysOfWeek = Seq("ponedelnik", "vtornik", "srqda", "chetvurtuk", "petuk", "subota", "nedelq")
      val today = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withMillisOfDay(0)
      val doc = utils.loadXML( """http://bnt.bg/programata""")
      val dayReg = """(.*) (.*) (\d\d\d\d) (.*)""".r
      val weeklyProgram = (doc \\ "li").filter(it => (it \ "@class").text == "programDays" && (it \ "@id").text.endsWith("_w")).flatMap{ it =>
        val items = (it\\"div").filter{ it => (it\"@class").text=="programInnerWholeCell"}
        val dayReg(dayStr, monthStr, yearStr,_) = (items.head\\"div").find(it2 => (it2\"@class").text=="programInnerNameInfo").get.text
        val day = new DateTime(yearStr.toInt, utils.months.indexOf(monthStr)+1, dayStr.toInt, 0,0,0,0, DateTimeZone.forID("Europe/Sofia"))
        if (day.isBefore(today)) Nil
        else utils.adjustArticles(items.tail.map{
          //case <div>{_}<div>{timeStr}</div>{_}{titleStr @ _*}</div> =>
          case <div>{_}<div>{timeStr}</div>{_}<div>{titleNodes @ _*}</div>{_}</div> =>
            val (utils.time_hh_mm(h,m), title, details) = (timeStr.text, titleNodes.head.text.trim, titleNodes.tail.text.trim)
            Article(this, day.plusMinutes(h.toInt * 60 + m.toInt), day, title, title, Some(details))
        })
      }
      enrich(weeklyProgram, bntw_izbrano)
    }
  }

  object NovaTV extends Station(
    config.getString("stations.NovaTV.name"),
    config.getString("stations.NovaTV.prefix"),
    //"stations.NovaTV.url",
    config.getString("stations.NovaTV.destination"),
    config.getInt("stations.NovaTV.leftPadding"),
    config.getInt("stations.NovaTV.rightPadding")
  ) {

    override val factory: Reader[Article,Try[String]] = Reader(utils.rtmpVideo(Map(
      "-r" -> "rtmp://e1.cdn.bg:2060/fls",
      "-a" -> "fls",
      "-f" -> "WIN 11,7,700,224",
      "-W" -> "http://i.cdn.bg/eflash/jwNTV/jplayer.swf",
      "-p" -> "http://i.cdn.bg/live/0OmMKJ4SgY",
      "-y" -> "ntv_1.stream",
      "-T" -> "N0v4TV6#2",
      "-S" -> "192.168.1.10:1080"
    )))

    def programa = {
      val today = DateTime.now(DateTimeZone.forID("Europe/Sofia")).withMillisOfDay(0)
      utils.adjustArticles(0.to(6).flatMap{step =>
        val date = today.plusDays(step)
          val dailyDoc = utils.loadXML(s"http://novatv.bg/schedule/index/${date.year().get}/${date.monthOfYear().get}/${date.dayOfMonth().get}/")
          val list = (dailyDoc \\ "ul").find(it => (it \ "@class").text == "timeline novatv").head \ "li"
          list.map{
            case <li>{_}<div>{timeStr}</div>{_}<a>{titleStr}</a>{items @ _*}</li> =>
              val (utils.time_hh_mm(h,m), title) = (timeStr.text, titleStr.text)
              Article(this, date.plusMinutes(h.toInt*60+m.toInt), date, title, title, None)
          }
      })
    }
  }

  val stations = Seq(Horizont, HristoBotev, NovaTV, BntWorld)

  def refreshAll = {
    val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
    val end = start.plusHours(utils.config.getInt("refreshWindowSize"))

    val reg = """(.*) (.*)""".r
    val picksFilename = utils.config.getString("picks")
    val picks = scala.io.Source.fromFile(picksFilename).getLines().map{case reg(a,b) => (a,b.toLong)}.toList.filter{case (_,b) => b>=start.getMillis}
    val writer = new PrintWriter(new File(picksFilename))
    writer.write(picks.map(it => s"${it._1} ${it._2}\n").mkString)
    writer.close()

    import collection.JavaConversions._

    refreshResults = utils.config.getStringList("active_stations").toList.map{ name =>
      val station = stations.find(_.name==name).get
      (station, station.refresh(start, end, picks))
    }
    refreshTime = start
    end
  }

  def main(args: Array[String]): Unit = {
    new Thread { override def run = ApiBoot.main(Array())}.start
//    Future { ApiBoot.main(Array()) }

    while(true) {
      nextRefreshTime = refreshAll
      Thread.sleep(Seconds.secondsBetween(DateTime.now, nextRefreshTime).getSeconds*1000L)
    }
  }

}
