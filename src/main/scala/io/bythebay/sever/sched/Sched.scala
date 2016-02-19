package io.bythebay.sever.sched

import java.io._
import java.util.Locale

import io.bythebay.excel.Implicits._
import io.bythebay.sever.cards.{IndexCardProject, ScheduledTalk, TalkKey}
import io.bythebay.sever.talk.Talk
import io.bythebay.util.sever.showMaybe
import org.apache.poi.ss.util._
import org.apache.poi.xssf.usermodel._
import org.joda.time.{LocalDate, DateTime}

/**
 * Created by a on 5/26/15.
 */

abstract class ExcelSchedSpec(excelIn: String, rowBase: Int) {
  def talkRow(st: ScheduledTalk, talkIndex: Integer): Unit
}

class ExcelSched(excelIn: String, rowBase: Int)
  extends ExcelSchedSpec(excelIn, rowBase) {

  val wb = new XSSFWorkbook(new FileInputStream(excelIn))
  val sheet = wb.getSheetAt(0)

  val javaDateFormat = "yyyy-MM-dd HH:mm"
  val dateFormat = new java.text.SimpleDateFormat(javaDateFormat)

  // date cell style 1
  val excelFormatPattern = DateFormatConverter.convert(Locale.US,javaDateFormat)
  val cellStyleDate1 = wb.createCellStyle()
  val poiFormat = wb.createDataFormat()
  cellStyleDate1.setDataFormat(
    poiFormat.getFormat(excelFormatPattern))

  // date cell style 2
  val createHelper = wb.getCreationHelper()
  val cellStyleDate2 = wb.createCellStyle()
  cellStyleDate2.setDataFormat(
    createHelper.createDataFormat().getFormat("m/d/yy h:mm"))

  override def talkRow(st: ScheduledTalk, talkIndex: Integer): Unit = {

    val r = sheet.createRow(rowBase+talkIndex)

    val startFinish = st.slot.asList map (time => (st.date, time))

    r.createCellA('a').setCellValue(st.key.toString)
    r.createCellA('b').setCellValue(showMaybe(st.talk.map(_.title)))
    r.createCellA('c').setCellValue("Y")

    startFinish.zipWithIndex foreach { case ((date,time), i) =>
      val c = r.createCell(3 + i)
      c.setCellValue(dateFormat.parse(s"$date $time"))
      c.setCellStyle(cellStyleDate1)
    }

    val track = st.key.track.toString // Char gets uploaded as an integer

    r.createCellA('f').setCellValue(track)
    r.createCellA('i').setCellValue(showMaybe(st.talk.map(_.body)))
    r.createCellA('j').setCellValue(showMaybe(st.talk.map(_.speaker.name)))
    r.createCellA('p').setCellValue(track) // venue same as track for now
  }

  def write(excelOut: String): Unit = {
    val fileOut = new FileOutputStream(excelOut)
    wb.write(fileOut)
    fileOut.close()
  }
}


object Sched {

  case class Params(talksFile:  String,
                    fromDay:    LocalDate,
                    toDay:      LocalDate,
                    excelIn:    String,
                    excelOut:   String,
                    dayLetters: Array[Char],
                    cardFiles:  Array[String]) {

  }
  object Params {
    def apply(args: Array[String]): Params = {
      val letters = args(5).toCharArray
      val cardFiles = args.drop(6)
      assert(letters.size == cardFiles.size, s"number of letters (${letters.size}) in ${letters.mkString(",")} "+
      s"must correspond to the number of card files (${cardFiles.size}): ${cardFiles.mkString(",")}")

      val dir = "/l/dbtb/data/"
      new Params(
        talksFile  = dir + args(0),
        fromDay    = new LocalDate(args(1)),
        toDay      = new LocalDate(args(2)),
        excelIn    = dir + args(3),
        excelOut   = dir + args(4),
        dayLetters = letters,
        cardFiles  = cardFiles map (dir + _)
      )
    }
  }

  def main(args: Array[String]): Unit = {

    val par = Params(args)

    println(s"reading talks from ${par.talksFile}, sessions from ${par.excelIn}, writing ${par.excelOut}, days: " + par.cardFiles.mkString(", "))

    val cardProject = IndexCardProject(
        name="pipelines", letter='P',
        cardsFile=par.cardFiles.head,
        talksFile=par.talksFile,
        date=par.fromDay
      )

    val excelSched = new ExcelSched(par.excelIn, 8)

    cardProject.schedule.zipWithIndex foreach { case (scheduledTalk, i) =>
      excelSched.talkRow(scheduledTalk, i)
    }

    excelSched.write(par.excelOut)
  }
}
