package io.bythebay.sever.cards

import java.io.PrintWriter

import io.bythebay.sever.cards.Plist.{Pint, Pstring, Pbool, Pleaf}
import io.bythebay.sever.talk.{Talk, Summary}
import io.bythebay.util.sever.{so,showMaybe}

import scala.xml.{Elem, XML}

/**
  * Created by a on 2/13/16.

  * import scalatags.generic
  * import scalatags.text._
  * import acyclic.file

  * trait CardTags extends generic.Tags2[Builder, String, String]{
  * // Document Metadata
  * lazy val dict = "dict".tag
  * lazy val key  = "key".tag
  * lazy val array = "array".tag
  * }
  */

// TODO doesn't work, needs scalatags extension
object tags {

  def tag(prefix: String, indent: String = "  ")(body: => List[String]): List[String] = {
    val bs = body map (indent+_)

    s"<$prefix>" +: bs :+ s"</$prefix>"
  }

  def tagLeaf(prefix: String, indent: String = "  ")(body: => String): List[String] = {

    s"<$prefix>$body</$prefix>" :: Nil
  }

  val dict = tag("dict") _
  def key(s: String): List[String] = tagLeaf("key")(s)

  //    val level = 1
  //    val xmll =
  //      dict {
  //        key { "privet" }
  //        key { "hello" }
  //      }
  //
  //    val xml = xmll.mkString
}

object Plist {


  // Apple's plist, not a PList

  abstract trait Pleaf

  sealed case class Pstring(s: String) extends Pleaf

  sealed case class Pint(i: Int) extends Pleaf

  sealed case class Pbool(b: Boolean) extends Pleaf

  type Ptuple = (String, scala.xml.Node)

  object Pleaf {
    def apply(v: scala.xml.Node): Pleaf = {
      v.label match {
        case "string"  => Pstring(v.text)
        case "integer" => Pint(v.text.toInt)
        case "true"    => Pbool(true)
        case "false"   => Pbool(false)
        case _ => throw new IllegalArgumentException(s"an unexpected leaf XML type: $v")
      }
    }
  }

  type Pmap = Map[String, Pleaf]

  def apply(c: xml.Node): Pmap = {
    val (ks, vs) = c.child partition (_.label=="key")
    (ks map (_.text)) zip (vs map (Pleaf(_))) toMap
  }

}


abstract trait KindTalk
case object FullTalk    extends KindTalk { override def toString = "40"}
case object HalfTalk    extends KindTalk { override def toString = "20"}
case object KeynoteTalk extends KindTalk { override def toString = "KN"}
object KindTalk {
  def apply(kind: String): KindTalk =
    kind match {
          case ""  => FullTalk
          case "H" => HalfTalk
          case "K" => KeynoteTalk
          case _ => throw new IllegalArgumentException(s"bad kind of talk: $kind")
    }
}

case class TalkTime( h: Int, m: Int = 0) {
  if (h < 0 || h > 12 || m < 0 || m >= 60 || m % 5 != 0)
    throw new IllegalArgumentException(s"hour/minutes are wrong: $this")

  // the only use case for now is doubling half-talks followed by a blank card
  def addTime(hours: Int = 0, minutes: Int = 20): TalkTime = {
    val mm = m + minutes
    val h2 = mm / 60
    val m2 = mm - h2 * 60
    TalkTime(h+h2, m2)
  }

  override def toString = {
    val mm = ('0'+:s"$m").takeRight(2)
    val hh = (' '+:s"$h").takeRight(2)
    s"$hh:$mm"
  }
}

object TalkTime {
  def apply(t: String): TalkTime =
    try {
      val a = t split ":"
      new TalkTime(a(0).toInt, a(1).toInt)
    }
    catch {
      case _: Throwable => throw new IllegalArgumentException(s"bad talk time: $t")
    }

  def range(r: String): (TalkTime, TalkTime) = try {
    val a = r split "-"
    a match {
      case Array(s,f) => (TalkTime(s), TalkTime(f))
      // TODO ensure TalkTime(s) < TalkTime(f)
    }
  }

  catch {
    case _: Throwable => throw new IllegalArgumentException(s"bad talk range: $r")
  }
}

case class TalkSpan(slot: Char, start: TalkTime, finish: TalkTime, kind: KindTalk) {
  override def toString = s"$slot $start-$finish $kind"
}

object TalkSpan {
  def apply(s: String): TalkSpan = try {
    val parts = s split " "
    val (start, finish) = TalkTime.range(parts(1))
    val c = parts(0).head // must be single character
    parts match {
      case Array(_, _, k) => new TalkSpan(c, start, finish, KindTalk(k))
      case Array(_, _)    => new TalkSpan(c, start, finish, FullTalk)
    }
  }
  catch {
    case _: Throwable => throw new IllegalArgumentException(s"bad talk span: $s")
  }
}

abstract trait CardColor
case object White   extends CardColor
case object Green   extends CardColor
case object Yellow  extends CardColor
case object Orange  extends CardColor
case object Red     extends CardColor
case object Purple  extends CardColor
case object Blue    extends CardColor
case object Brown   extends CardColor
case object Cyan    extends CardColor
case object Magenta extends CardColor
case object Pink    extends CardColor
case object Lime    extends CardColor
case object Cowboy  extends CardColor
case object Corsa   extends CardColor
object CardColor {
  val colors = List(
    White, Green, Yellow, Orange, Red,
    Purple, Blue, Brown, Cyan, Magenta,
    Pink, Lime, Cowboy, Corsa)

  val names = colors map (_.toString)

  val c2s = colors zip names  toMap
  val s2c = names  zip colors toMap

  def apply(s: String): CardColor = s2c.get(s) match {
    case Some(c) => c
    case _ => throw new IllegalArgumentException(s"no CardColor for color $s")
  }
}


case class IndexCard(draft: Boolean = true,
                     label: CardColor = White,
                     relation: Option[String] = None,
                     sortOrder: Int,
                     stack: Option[Boolean] = None,
                     synopsis: Option[String] = None,
                     text: Option[String] = None,
                     title: String) {
  def truthXML(cond: Boolean) = if (cond) <true/> else <false/>
  def toXML =
  <dict>
    <key>draft</key>
    {truthXML(draft)}
    <key>label</key>
    <string>{label}</string>
    <key>sortOrder</key>
    <integer>{sortOrder}</integer>
    <key>stack</key>
    <false/>
    <key>title</key>
    <string>{title}</string>
    <key>synopsis</key>
    <string>{synopsis}</string>
    <key>text</key>
    <string>{text}</string>
  </dict>

  def isBlank:  Boolean = title.trim.isEmpty
  def nonBlank: Boolean = !isBlank

//  def showDraft = if (draft) 'D' else '-'
  val showRelation = showMaybe(relation)
//  val showStack = stack.map(cond => s", stack $cond").getOrElse("")
  val showSynoposis = showMaybe(synopsis, ", synopsis ").take(30)
  val showText     = showMaybe(text,     ", text ").take(30)
  override def toString = s"$label\t$showRelation $sortOrder $title"
//    s"draft $draft, label $label$showRelation$showStack, $title$showSynoposis$showText"
}


object IndexCard {

  def apply(s: Summary, i: Int): IndexCard =
    new IndexCard(
      draft=true, label=White, relation=None, sortOrder=i, stack=None,
      synopsis = so(s.synopsis), text=so(s.body), title=s.headline)


  def apply(c: scala.xml.Node): IndexCard = {
    val m = Plist(c)

    // ks: draft, label, relation, sortOrder, stack, synopsis, text, title
    val draft     = m("draft")         match { case Pbool(b)         => b
                    case _ => throw new IllegalArgumentException("bad draft") }
    val label     = m("label")         match { case Pstring(s)       => s
                    case _ => throw new IllegalArgumentException("bad label") }
    val color     = CardColor(label)
    val relation  = m.get("relation")  match { case Some(Pstring(s)) => Some(s)
                    case None => None
                    case _ => throw new IllegalArgumentException("bad relation") }
    val sortOrder = m("sortOrder")     match { case Pint(i)          => i
                    case _ => throw new IllegalArgumentException("bad sortOrder") }
    val stack     = m.get("stack")     match { case Some(Pbool(b))   => Some(b)
                    case None => None
                    case _ => throw new IllegalArgumentException("bad stack") }
    val synopsis  = m.get("synopsis")  match { case Some(Pstring(s)) => Some(s)
                    case None => None
                    case _ => throw new IllegalArgumentException("bad synopsis") }
    val text      = m.get("text")      match { case Some(Pstring(s)) => Some(s)
                    case None => None
                    case _ => throw new IllegalArgumentException("bad text") }
    val title     = m("title")         match { case Pstring(s)       => s
                    case _ => throw new IllegalArgumentException("bad title") }

    new IndexCard(draft, color, relation, sortOrder, stack, synopsis, text, title)
  }
}

case class Label(color: CardColor, sortOrder: Int, description: String) {
  override def toString =
    s"label $color\t$sortOrder $description"
}

object Label {
  def apply(c: xml.Node): Label = {
    val m = Plist(c)

    val color           = m("colorNameKey")    match { case Pstring(s) => CardColor.s2c(s)
                    case _ => throw new IllegalArgumentException("bad colorNameKey") }
    val sortOrder       = m("sortOrder")       match { case Pint(i)    => i
                    case _ => throw new IllegalArgumentException("bad sortOrder") }
    val userDescription = m("userDescription") match { case Pstring(s) => s
                    case _ => throw new IllegalArgumentException("bad userDescription") }

    new Label(color, sortOrder, userDescription)
  }
}

case class TalkKey(day: Char, slot: Char, track: Char) {
  override def toString = s"$day$slot$track"
}

case class ScheduledTalk(card: IndexCard, slot: TalkSpan, key: TalkKey)


case class IndexCardProject(name: String, letter: Char, cards: Seq[IndexCard] = Nil, labels: Seq[Label] = Nil) {
  import IndexCardProject._

  val cardFileName = fileName(name)
  val cardPathName = pathName(name)

  def write(pathname: String = cardPathName): Unit = {
    val realPathName = s"x-$pathname"
    print(s"Writing to $realPathName...  ")
    val cardFile = new PrintWriter(realPathName)

    cardFile.write(s"$header1$name$header2")

    cards
      //      .take(1)
      .foreach { case card =>
      println(card)
      cardFile.write(card.toXML.toString)
    }

    cardFile.write(footer)
    cardFile.close()
    println(s"Wrote ${cards.size} cards to $realPathName.")
  }

  val times = labels filterNot(_.color==White) map { case Label(color,_,description) =>
    (color, TalkSpan(description))
  } toMap

  val schedule = cards.zipWithIndex collect { case (card, i) if card.nonBlank =>

    val slot  = times(card.label)
    val slot2 = if (i < cards.size-1 && cards(i+1).isBlank)
        slot.copy(finish=slot.finish.addTime(minutes=20))
      else
        slot

    val talkKey = TalkKey(letter, slot.slot, card.relation.get.head)

    ScheduledTalk(card, slot2, talkKey)
  }
}


object IndexCardProject {

  def fileName(name: String) = s"$name.indexcard"
  // /Users/a/Dropbox/IndexCard
  def pathName(name: String) = s"/Users/a/IndexCard/${fileName(name)}"

  def apply(name: String, letter: Char, pathname: String): IndexCardProject = {
    println(s"Loading IndexCard project from $pathname")
    val x = XML.loadFile(pathname)
    val a = x \\ "array"

    val cs = a(0).child \\ "dict"
    val ls = a(1).child \\ "dict"

    val cards  = cs map (IndexCard(_))
    val labels = ls map (Label(_))

    new IndexCardProject(name, letter, cards, labels)
  }

  // splitting header into two to avoid having to commit to a name in the middle just yet
  val header1 =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
       |<plist version="1.0">
       |<dict>
       |	 <key>name</key>
       |	 <string>""".stripMargin
  // name
  val header2 =
       """</string>
       |	 <key>sortOrder</key>
       |	 <integer>1</integer>
       |  <key>cardList</key>
       |	 <array>
      """.stripMargin

  val footer =
    """
      |	</array>
      |	<key>colorLabelList</key>
      |	<array>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>White</string>
      |			<key>sortOrder</key>
      |			<integer>0</integer>
      |			<key>userDescription</key>
      |			<string>White</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Green</string>
      |			<key>sortOrder</key>
      |			<integer>1</integer>
      |			<key>userDescription</key>
      |			<string>Green</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Yellow</string>
      |			<key>sortOrder</key>
      |			<integer>2</integer>
      |			<key>userDescription</key>
      |			<string>Yellow</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Orange</string>
      |			<key>sortOrder</key>
      |			<integer>3</integer>
      |			<key>userDescription</key>
      |			<string>Orange</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Red</string>
      |			<key>sortOrder</key>
      |			<integer>4</integer>
      |			<key>userDescription</key>
      |			<string>Red</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Purple</string>
      |			<key>sortOrder</key>
      |			<integer>5</integer>
      |			<key>userDescription</key>
      |			<string>Purple</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Blue</string>
      |			<key>sortOrder</key>
      |			<integer>6</integer>
      |			<key>userDescription</key>
      |			<string>Blue</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Brown</string>
      |			<key>sortOrder</key>
      |			<integer>7</integer>
      |			<key>userDescription</key>
      |			<string>Brown</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Cyan</string>
      |			<key>sortOrder</key>
      |			<integer>8</integer>
      |			<key>userDescription</key>
      |			<string>Cyan</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Magenta</string>
      |			<key>sortOrder</key>
      |			<integer>9</integer>
      |			<key>userDescription</key>
      |			<string>Magenta</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Pink</string>
      |			<key>sortOrder</key>
      |			<integer>10</integer>
      |			<key>userDescription</key>
      |			<string>Pink</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Lime</string>
      |			<key>sortOrder</key>
      |			<integer>11</integer>
      |			<key>userDescription</key>
      |			<string>Lime</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Cowboy</string>
      |			<key>sortOrder</key>
      |			<integer>12</integer>
      |			<key>userDescription</key>
      |			<string>Cowboy</string>
      |		</dict>
      |		<dict>
      |			<key>colorNameKey</key>
      |			<string>Corsa</string>
      |			<key>sortOrder</key>
      |			<integer>13</integer>
      |			<key>userDescription</key>
      |			<string>Corsa</string>
      |		</dict>
      |	</array>
      |</dict>
      |</plist>
    """.stripMargin


  val card1verbatim =
    """
      |		<dict>
      |			<key>draft</key>
      |			<true/>
      |			<key>label</key>
      |			<string>White</string>
      |			<key>sortOrder</key>
      |			<integer>0</integer>
      |			<key>stack</key>
      |			<false/>
      |			<key>synopsis</key>
      |			<string>Deep Learning's Bag of Tricks
      |</string>
      |			<key>title</key>
      |			<string>1: Chris Moody</string>
      |		</dict>
      |
    """.stripMargin
}

object CreateProject {
  import IndexCard._

  def main(args: Array[String]) = {

    val talks = Talk.readFromTSV(args(0))
//    val talk1 = talks.headOption.get

    val cardFileName = args(1)
    val tagDays = List("law", "democracy", "life", "ux", "aiot", "text", "general") // general captures the rest
    val tagLetters = "WDLUATG".toCharArray

    val days = Talk.talkDays(tagDays, talks)

    (days zip tagLetters) foreach { case ((day, talks), letter)  =>
        val cards = talks.zipWithIndex map { case (talk, i) =>  IndexCard(talk.summary, i) }
        val cardProject = IndexCardProject(day, letter, cards)
        cardProject.write()
    }
  }
}

object LoadProject {
  import IndexCardProject._

  def main(args: Array[String]) = {

    val day       = "text"
    val dayLetter = 'T'

    val cardPathName = pathName(day)

    val cardProject = IndexCardProject(day, dayLetter, cardPathName)

    println("Cards:")
    cardProject.cards foreach (println(_))

    println("Labels:")
    cardProject.labels foreach (println(_))

    println("Schedule:")
    cardProject.schedule foreach { case ScheduledTalk(talk, slot, talkKey) =>
        println(s"$talkKey $slot ===>>> $talk")
    }
  }
}