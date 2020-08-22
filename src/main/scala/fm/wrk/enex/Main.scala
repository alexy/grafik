package fm.wrk.enex

/**
  * Created by alexy on 6/17/17.
  */

import com.lucidchart.open.xtract.{ParseSuccess, XmlReader}

import scala.xml.XML

object Main {

  //private val xml = XML.load(getClass.getResourceAsStream("/enex3.xml"))

  def main(args: Array[String]): Unit = {
    val xml = XML.loadFile(args(0))
    val parsedEnex = XmlReader.of[Enex].read(xml)

    val rowsOpt = parsedEnex match {
     case ParseSuccess(s) => // println(s.taggedOnly)
          val RowNumber = "([0-9]+): .*".r
          val ns = s.notes.notes.map(_.title).collect { 
            // println("++" +title)
            _ match { case RowNumber(n) => n }
          } map(_.toInt)
          Some(ns.sorted)
      case _ => None
      }

      println(rowsOpt)
  }
}
