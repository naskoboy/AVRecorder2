import java.io.File
import java.util.{TimerTask, Timer}
import com.typesafe.config.ConfigFactory
import org.joda.time.format.DateTimeFormat
import org.joda.time.{Seconds, Minutes, DateTimeZone, DateTime}
import scala.util.Try
import scala.xml.Node

/**
 * Created by nasko on 11/20/2015.
 */

object utils {

  val time_reg = ".\u0442 (\\d{1,2}).(\\d\\d) \u0434. (\\d{1,2}).(\\d\\d) \u0447\u0430\u0441\u0430".r // от (\d{1,2}).(\d\d) до (\d{1,2}).(\d\d) часа
  val day_reg = "(.*), (\\d{1,2}) (.*)".r
  val day_reg_long = """(\d{1,2}) (.*) (\d\d\d\d), (.*)""".r
  val time_hh_mm = """(\d\d):(\d\d)""".r
  val timeslot_reg = """(\d\d):(\d\d) (\d\d):(\d\d) (.*)""".r

  // https://www.branah.com/unicode-converter
  val months = Seq(
    """\u044f\u043d\u0443\u0430\u0440\u0438""",
    """\u0444\u0435\u0432\u0440\u0443\u0430\u0440\u0438""",
    """\u043c\u0430\u0440\u0442""",
    """\u0430\u043f\u0440\u0438\u043b""",
    """\u043c\u0430\u0439""",
    """\u044e\u043d\u0438""",
    """\u044e\u043b\u0438""",
    """\u0430\u0432\u0433\u0443\u0441\u0442""",
    """\u0441\u0435\u043f\u0442\u0435\u043c\u0432\u0440\u0438""",
    """\u043e\u043a\u0442\u043e\u043c\u0432\u0440\u0438""",
    """\u043d\u043e\u0435\u043c\u0432\u0440\u0438""",
    """\u0434\u0435\u043a\u0435\u043c\u0432\u0440\u0438"""
  )
  val published_reg = "\u043f\u0443\u0431\u043b\u0438\u043a\u0443\u0432\u0430\u043d\u043e \u043d\u0430 (\\d\\d).(\\d\\d).(\\d\\d)(.*)".r
  val ymdHM_format = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  val HM_format = DateTimeFormat.forPattern("HH:mm")

  val config = ConfigFactory.load()
  val vlc = config.getString("vlc")

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

  def audioRecorder(station_prefix: String, url: String, destination_folder: String)(article: Article) = Try {
    val timestamp_format = DateTimeFormat.forPattern("yyMMdd_HHmm")
    val filename = s"${station_prefix}_${utils.getFixedString(article.title)}_${timestamp_format.print(article.start)}"
    val fullFileName = s"""$destination_folder\\$filename.mp3"""
    import sys.process._
    val cmd = s""""${utils.vlc}" $url --sout #duplicate{dst=std{access=file,mux=raw,dst="$fullFileName"}} --run-time=7200 -I dummy --dummy-quiet vlc://quit"""
    val process = cmd.run
    Thread.sleep(1000*Seconds.secondsBetween(DateTime.now, article.end).getSeconds)
    process.destroy()

    // fix mp3 tag, https://github.com/soc/jaudiotagger
    import org.jaudiotagger.audio.AudioFileIO
    import org.jaudiotagger.tag.{FieldKey, TagOptionSingleton}
    import org.jaudiotagger.tag.id3.valuepair.TextEncoding
    TagOptionSingleton.getInstance.setId3v23DefaultTextEncoding(TextEncoding.getInstanceOf.getIdForValue("UTF-8").byteValue())

    val tag = new org.jaudiotagger.tag.id3.ID3v23Tag()
    tag.addField(FieldKey.TITLE, article.title)
    article.details.map(tag.addField(FieldKey.COMMENT, _))
    val file = AudioFileIO.read(new File(fullFileName))
    file.setTag(tag)
    file.commit
  }

}

case class Article(start: DateTime, end: DateTime, title: String, details: Option[String]) {
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

}

abstract class Station(name: String, cycleHours: Int, recorder: Article => Try[Unit]) {
  def programa: List[Article]

  def getSubscriptions = {
    val picker_reg = """([^ ]*) (.*)""".r
    val timeslot_reg = """(.*) TimeSlot (\d) (\d\d):(\d\d) (\d\d):(\d\d) (.*)""".r
    val subscriptions = scala.io.Source.fromFile(utils.config.getString("subscriptions")).getLines().toList
    subscriptions.foldLeft(List.empty[String], List.empty[Article]){ (acc,it) =>
      it match {
        case timeslot_reg(station,day,h1,m1,h2,m2,title) if (station == name) =>
          val now = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
          val nextDay = now.plusDays((day.toInt-now.dayOfWeek().get()+7)%7)
          (acc._1, Article(nextDay.withHourOfDay(h1.toInt).withMinuteOfHour(m1.toInt), nextDay.withHourOfDay(h2.toInt).withMinuteOfHour(m2.toInt), title, None) :: acc._2)
        case picker_reg(station,words) if (station == name) =>
          (words :: acc._1, acc._2)
      }}
  }

  def pick(start: DateTime, end: DateTime) = {
    val (pickers, timeslots) = getSubscriptions
    (programa.filter(article => pickers.exists(article.title.indexOf(_) >= 0)) ++ timeslots).filter(it => it.start.isAfter(start) && it.start.isBefore(end))
  }

  def go {
    new Thread {
      override def run: Unit = {
        while(true) {
          val start = DateTime.now(DateTimeZone.forID("Europe/Sofia"))
          val end = start.plusHours(cycleHours)
          pick(start,end).foreach { article => {
            println(article)
            new Timer().schedule(
              new TimerTask { override def run(): Unit = recorder(article)},
              article.start.toDate
            )}}
          Thread.sleep(Seconds.secondsBetween(DateTime.now, end).getSeconds*1000L)
        }
      }
    }.start
  }

}


object Boot {

  /*
      def getPID(appName:String, filename:String) = scala.io.Source.fromInputStream(Runtime.getRuntime()
        .exec("wmic PROCESS WHERE \"Caption='" + appName + "' AND CommandLine like '%" + filename + "%'\" GET ProcessId /FORMAT:list")
        .getInputStream).getLines.filter(it => it.indexOf("ProcessId")>=0).next.substring(10)

      def toCommand(cmd: Seq[String]) = cmd.map(it => if (it.indexOf(" ") >=0) s""""$it"""" else it).mkString(" ")

      def executeCommand(cmd: String) = {
        import sys.process._
        val stdout = new StringBuilder
        val stderr = new StringBuilder
        val status = Process(cmd) ! ProcessLogger(it => { stdout append (it+"\n") }, it => { stderr append (it+"\n")})
        (status, stderr.toString(), stdout.toString())
      }

      def executeShellCommand(cmd: String) = executeCommand(s"cmd /K $cmd")
  */

  // https://github.com/chris-twiner/scalesXml
  // http://anti-xml.org/
  // https://github.com/mpatric/mp3agic

  def minusWords(a: String, b: String) = { val bb = b.split(" ") ; a.split(" ").filter(it => it.size>3 && !b.exists(_.equals(it))).size }
  def expandedTitle(a: String, b: String) = { val (r1, r2) = (minusWords(a,b), minusWords(b,a)) ; if (r1<r2) b else a }
  def expandedTime(a1: DateTime, a2: DateTime, b1: DateTime, b2: DateTime) = (if (a1.isBefore(b1)) a1 else b1, if (a2.isAfter(b2)) a2 else b2)

  def hb_izbrano(url: String) = {

    def strip(node:Seq[Node]): Seq[Node] = node.flatMap{
      case <br/> => Nil
      case <p>{items @ _*}</p>       => strip(items)
      case <span>{items @ _*}</span> => strip(items)
      case <div>{items @ _*}</div>   => strip(items)
      case it => Seq(it)
    }
    val doc = utils.loadXML(url)
    val utils.published_reg(pDateStr,pMonthStr,pYearStr,_) = (doc \\ "span").find(it => (it \ "@itemprop").text == "datePublished").get.text
    val pDate = new DateTime(2000+pYearStr.toInt, pMonthStr.toInt, pDateStr.toInt,0,0, DateTimeZone.forID("Europe/Sofia"))

    val news_title = (doc \\ "div").find(it => (it \ "@class").text == "news_title").get
    val (dayNode, postedNode, cont) = news_title match { case <div>{_}<h1>{day @ _*}</h1>{_}<div>{date @ _*}</div>{_}<div>{cont @ _*}</div>{w @ _*}</div> => (day, date, cont)}
    val utils.day_reg(dayStr, dateStr, monthStr) = dayNode.text

    val datetime = {
      val candidate = new DateTime(2000+pYearStr.toInt, utils.months.indexOf(monthStr)+1, dateStr.toInt, 0, 0, DateTimeZone.forID("Europe/Sofia"))
      if (candidate.isAfter(pDate)) candidate
      else new DateTime(2000+pYearStr.toInt+1, utils.months.indexOf(monthStr)+1, dateStr.toInt, 0, 0, DateTimeZone.forID("Europe/Sofia"))
    }

    val cont_flat = strip(cont)
    val locations = cont_flat.foldLeft((List.empty[Int],0))((acc,it) => it match { case <b>{_}{some @ _*}</b> => (acc._2-1 :: acc._1, acc._2+1) case _ => (acc._1, acc._2+1) })._1.reverse.drop(1)
    val location_splits = locations.zip(locations.drop(1) ++ List(cont_flat.size))
    val items = location_splits.map(it => cont_flat.drop(it._1).take(it._2-it._1))
    items.map{ case date :: title :: details =>
      val (t1, t2) = date.text match {
        case utils.time_reg(h1,m1,h2,m2) => (h1.toInt*60+m1.toInt, h2.toInt*60+m2.toInt)
        case _ => (0,0)
      }
      Article(datetime.plusMinutes(t1), datetime.plusMinutes(t2), title.text, Some(details.map(_.text).mkString("\n")))
    }
  }

  def bnr_sedmichna_programa(doc: Node) = {

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
        Article(datetime.plusMinutes(h.toInt*60+m.toInt), datetime, title, None)
      }
      (articles.zip(articles.tail ++ List(Article(datetime.plusDays(1),datetime,"",None)))).map{ case (a1,a2) =>  a1.copy(end = a2.start) }
    }}
  }

  import utils.config
  object HristoBotev extends Station("HristoBotev", utils.config.getInt("stations.HristoBotev.cycleHours"), utils.audioRecorder(config.getString("stations.HristoBotev.prefix"), config.getString("stations.HristoBotev.url"), config.getString("stations.HristoBotev.destination"))) {
    def programa = {
      val weeklyDoc = utils.loadXML("""http://bnr.bg/hristobotev/page/sedmichna-programa""")
      val weeklyProgram = bnr_sedmichna_programa(weeklyDoc)
      val izbrano = ((weeklyDoc \\ "div").find(it => (it \ "@class").text == "row-fluid module_container").get \\ "a").flatMap(it => hb_izbrano("http://bnr.bg" + it \ "@href")).sortWith((a,b) => a.start.isBefore(b.start))
      val enrichedWeeklyProgram = weeklyProgram.map(it => it.correspondenceArticle(izbrano) match { case Some(izb) => val (t1,t2) = expandedTime(it.start,it.end,izb.start,izb.end) ; Article(t1,t2,expandedTitle(it.title,izb.title),izb.details) case None => it } )
      enrichedWeeklyProgram
    }
  }

  object Horizont extends Station("Horizont", utils.config.getInt("stations.Horizont.cycleHours"), utils.audioRecorder(config.getString("stations.Horizont.prefix"), config.getString("stations.Horizont.url"), config.getString("stations.Horizont.destination"))) {
    def programa = {
      bnr_sedmichna_programa(utils.loadXML("""http://bnr.bg/horizont/page/programna-shema"""))
    }
  }

  def main(args: Array[String]) = {
    HristoBotev.go

    //val dailyProgram = hb_izbrano("""http://bnr.bg/hristobotev/post/100628587""")
    //val articles = hb_izbrano("""http://bnr.bg/hristobotev/post/100628497""")
    //val articles = hb_izbrano("""http://bnr.bg/hristobotev/post/100627160""")

    // val hb = bnr_sedmichna_programa("""http://bnr.bg/hristobotev/page/sedmichna-programa""")
    // val hr = bnr_sedmichna_programa("""http://bnr.bg/horizont/page/programna-shema""")

    //println(dailyProgram)
  }

}
