import scalaz._, Scalaz._, scalaz.concurrent._
import fastparse._

object Parser {

  case class Time(hours: Int, minutes: Int, seconds: Int, milliseconds: Int)
  case class Range(from: Time, to: Time)
  case class Line(index: Int, time: Range, subs: List[String])

  type Subtitles = List[Line]

  case class Duration(dMilliseconds: Int, dSeconds: Int, dMinutes: Int, dHours: Int)

  // Ordering Time
  implicit val timeOrder = new Order[Time] {
    def order(x: Time, y: Time): Ordering = {
      val orderHours = x.hours ?|? y.hours
      val orderMinutes = x.minutes ?|? y.minutes
      val orderSeconds = x.seconds ?|? y.seconds
      val orderMilliseconds = x.milliseconds ?|? y.milliseconds
      orderHours |+| orderMinutes |+| orderSeconds |+| orderMilliseconds
    }
  }

  // Showing subtitle lines.
  implicit object showTime extends Show[Time] {
    private def f(x: Int) = x match {
      case y if y < 10 => "0" ++ y.toString
      case y           => y.toString
    }
    private def g(x: Int) = x match {
      case y if y < 10  => "00" ++ y.toString
      case y if y < 100 => "0" ++ y.toString
      case y            => y.toString
    }
    override def shows(t: Time): String = t match {
      case Time(h, m, s, ml) => f(h) ++ ":" ++ f(m) ++ ":" ++ f(s) + "," ++ g(ml)
    }
  }

  implicit object showRange extends Show[Range] {
    override def shows(r: Range): String = r match {
      case Range(f, t) => f.shows ++ " --> " ++ t.shows
    }
  }

  implicit object showLine extends Show[Line] {
    override def shows(l: Line): String = l match {
      case Line(i, t, s) =>  List(i.toString, t.shows, s.mkString("\n")).mkString("\n") ++ "\n"
    }
  }

  val spaces = P(" ".rep)
  val digits = P(CharIn('0' to '9').rep(1).!).map(_.toInt)
  val separators = P(":" | ",")
  val newLine = P("\n")
  val doubleNewLine = P(newLine ~ newLine)
  val singleLine = P(CharsWhile(!"\n".contains(_)).!.rep(sep=newLine))
  val doubleLine = P(singleLine.!.rep(sep=newLine))
  val time = P(spaces.? ~ digits.rep(sep=separators) ~ spaces.?)
  val times = P(time.! ~ "-->" ~ time.!)

  def parse(s: String): Subtitles = {
    val Result.Success(result, _) = doubleLine.parse(s)
    result.filter(_ != "").flatMap(parseSingle).toList
  }

  def parseSingle(s: String): Option[Line] = {
    val Result.Success(result, _) = singleLine.parse(s)
    result match {
      case Seq(i, t, xs @ _*) => parseTimes(t).map(Line(i.toInt, _, xs.toList))
      case _                  => none[Line]
    }
  }

  def parseTimes(t: String): Option[Range] = {
    val Result.Success(result, _) = times.parse(t)
    (parseTime(result._1) |@| parseTime(result._2)) { Range }
  }

  def parseTime(t: String): Option[Time] = {
    val Result.Success(result, _) = time.parse(t)
    result match {
      case Seq(h, m, s, l, _*) => Time(h, m, s, l).some
      case _                   => none[Time]
    }
  }

  // Shifting time ranges.
  def shift(d: Duration, st: Subtitles): Subtitles =
    st.map(shiftLine(d, _))

  def shiftLine(d: Duration, l: Line): Line =
    l match {
      case Line(i, t, s) => Line(i, shiftRange(d, t), s)
    }

  def shiftRange(d: Duration, r: Range): Range =
    r match {
      case Range(f, t) => Range(shiftTime(d, f), shiftTime(d, t))
    }

  def shiftTime(d: Duration, t: Time): Time =
    (d, t) match {
      case (Duration(dl, ds, dm, dh), Time(th, tm, ts, tl)) =>
        val li = tl + dl
        val se = ts + ds + li / 1000
        val mi = tm + dm + se / 60
        val ho = th + dh + mi / 60
        Time(ho, mi % 60, se % 60, li % 1000)
    }

  def reindex(st: Subtitles): Subtitles =
    (st, Stream.from(1)).zipped map {
      case (Line(_, t, w), i) => Line(i, t, w)
    }

  def prefix(d: Duration, st: Subtitles): Subtitles =
    d match {
      case Duration(l, s, m, h) =>
        st.filter(_.time.from <= Time(h, m, s, l))
    }

  def suffix(d: Duration, st: Subtitles): Subtitles =
    d match {
      case Duration(l, s, m, h) =>
        val reindexed = reindex(st.filter(_.time.from > Time(h, m, s, l)))
        shift(Duration(-l, -s, -m, -h), reindexed)
    }

}

object Subtitles {

  import Parser._


  def readFile(filename: String): Task[String] = Task {
    import java.nio.charset.Charset
    import java.nio.charset.CodingErrorAction
    import scala.io.Source
    val decoder = Charset.forName("UTF-8").newDecoder()
    decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val source = Source.fromFile(filename)(decoder)
    val str = source.getLines().mkString("\n")
    source.close()
    str
  }

  def getArgs(args: Array[String]): Task[List[String]] =
    Task(args.toList)

  def writeHelp(): Task[Unit] = Task {
    println("USAGE:")
    println("subtitles show    <srt>")
    println("subtitles shift   <srt> <milliseconds> [<seconds> [<minutes> [<hours>]]]")
    println("subtitles reindex <srt>")
    println("subtitles prefix  <srt> <milliseconds> [<seconds> [<minutes> [<hours>]]]")
    println("subtitles suffix  <srt> <milliseconds> [<seconds> [<minutes> [<hours>]]]")
  }

  def printSubs(st: Subtitles): Task[Unit] =
    Task { st.foreach(x => println(x.shows)) }

  def transform(f: (Duration, Subtitles) => Subtitles, args: NonEmptyList[String], st: Subtitles): Subtitles =
    args match {
      case NonEmptyList(l, s, m, h, _*) => f(Duration(l.toInt, s.toInt, m.toInt, h.toInt), st)
      case NonEmptyList(l, s, m)        => f(Duration(l.toInt, s.toInt, m.toInt, 0), st)
      case NonEmptyList(l, s)           => f(Duration(l.toInt, s.toInt, 0, 0), st)
      case NonEmptyList(l)              => f(Duration(l.toInt, 0, 0, 0), st)
    }

  def withF(f: Subtitles => Subtitles, srt: String): Task[Unit] =
    readFile(srt) >>= printSubs _ <<< f <<< parse

  def main(args: Array[String]): Unit = {
    val program: Task[Unit] = for {
      args <- getArgs(args)
      _    <- args match {
        case "show"    :: srt :: Nil      => withF(identity, srt)
        case "shift"   :: srt :: x :: xs  => withF(transform(shift, x.wrapNel :::> xs, _), srt)
        case "reindex" :: srt :: Nil      => withF(reindex, srt)
        case "prefix"  :: srt :: x :: xs  => withF(transform(prefix, x.wrapNel :::> xs, _), srt)
        case "suffix"  :: srt :: x :: xs  => withF(transform(suffix, x.wrapNel :::> xs, _), srt)
        case _                            => writeHelp()
      }
    } yield ()
    // unleash the side-effects!
    program.handle { case e: Throwable => writeHelp().run }.run
  }

}
