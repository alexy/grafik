package io.bythebay.sever.cards

import java.io.PrintWriter

import io.bythebay.sever.talk.{Talk, Summary}


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

object CardSummary {

  var count=0

  implicit class Card(val summary: Summary) {

    def next: Unit = count += 1

    def toXML(sortOrder: Int = count) =
      <dict>
        <key>draft</key>
        <true/>
        <key>label</key>
        <string>White</string>
        <key>sortOrder</key>
        <integer>{count}</integer>
        <key>stack</key>
        <false/>
        <key>title</key>
        <string>{summary.headline}</string>
        <key>synopsis</key>
        <string>{summary.synopsis}</string>
        <key>text</key>
        <string>{summary.body}</string>
      </dict>

    override def toString = toXML().toString
  }
}

object IndexCard {
  def header(name: String) =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
       |<plist version="1.0">
       |<dict>
       |	 <key>name</key>
       |	 <string>$name</string>
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

object Main {
  import CardSummary._

  def main(args: Array[String]) = {

    val talks = Talk.readFromTSV(args(0))
//    val talk1 = talks.headOption.get

    val cardFileName = args(1)
    val tagDays = List("law", "democracy", "life", "ux", "aiot", "text", "general") // general captures the rest

    val days = Talk.talkDays(tagDays, talks)

    days foreach { case (day, talks) =>
        writeDay(day, talks)
    }
  }

  def writeDay(day: String, talks: List[Talk]): Unit = {
    val cardFileName = s"$day.indexcard"
    val cardPathName = s"/Users/a/Dropbox/IndexCard/$cardFileName"
    print(s"Writing to $cardPathName...  ")
    val cardFile = new PrintWriter(cardPathName)

    cardFile.write(IndexCard.header(cardFileName))

    talks
      //      .take(1)
      .foreach { case talk =>
      val card: Card = new Card(talk.summary)
      println(talk.summary.showTags)
      card.next
      cardFile.write(card.toString)
    }

    cardFile.write(IndexCard.footer)
    cardFile.close()
    println(s"Wrote ${talks.size} cards to $cardPathName.")
  }
}