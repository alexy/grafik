package fm.wrk.grafik.sched

import java.io._
import java.util.Locale

import fm.wrk.grafik.cards.{IndexCardProject, ScheduledTalk}
import fm.wrk.util.grafik.RichOptionString
import org.apache.poi.ss.util._
import org.apache.poi.xssf.usermodel._
import org.joda.time.{Period, LocalDate}

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

  override def talkRow(st: ScheduledTalk, row: Integer): Unit = {
    import fm.wrk.excel.Implicits._

    val r = sheet.createRow(row)

    val startFinish = st.slot.asList map (time => (st.date, time))

    r.createCellA('a').setCellValue(st.key.toString)
    r.createCellA('b').setCellValue(st.talk.map(_.title).getOrElse("TBD"))
    r.createCellA('c').setCellValue("Y")
    r.createCellA('d').setCellValue("N") // New in 2020

    startFinish.zipWithIndex foreach { case ((date,time), i) =>
      val c = r.createCell(4 + i)
      c.setCellValue(dateFormat.parse(s"$date $time"))
      c.setCellStyle(cellStyleDate1)
    }

    val track = st.key.track.toString // Char gets uploaded as an integer

    val label = track match {
      case "f" => "functional"
      case "r" => "reactive"
      case "d" => "data"
    }
    r.createCellA('g').setCellValue(label)
    r.createCellA('j').setCellValue(st.talk.map(_.body).show)
    r.createCellA('k').setCellValue(st.talk.map(_.speaker.name).show)
    r.createCellA('p').setCellValue(label) // venue same as track for now
  }

  def write(excelOut: String): Unit = {
    val fileOut = new FileOutputStream(excelOut)
    wb.write(fileOut)
    fileOut.close()
    println(s"finished writing Excel Sched schedule to $excelOut.")
  }
}


object Sched {

  case class Params(dir:        String,
                    talksFile:  String,
                    fromDay:    LocalDate,
                    toDay:      LocalDate,
                    excelIn:    String,
                    excelOut:   String,
                    dayLetters: Array[Char],
                    cardFiles:  Array[String]) {

  }
  object Params {
    

    def apply(dir: String, args: Array[String]): Params = {
      val letters = args(6).toCharArray
      val cardFiles = args.drop(7)
      assert(letters.size == cardFiles.size, s"number of letters (${letters.size}) in ${letters.mkString(",")} "+
      s"must correspond to the number of card files (${cardFiles.size}): ${cardFiles.mkString(",")}")

      new Params(
        dir        = dir,
        talksFile  = dir + args(1),
        fromDay    = new LocalDate(args(2)), // 2017-11-16
        toDay      = new LocalDate(args(3)),
        excelIn    = dir + args(4),
        excelOut   = dir + args(5),
        dayLetters = letters,
        cardFiles  = cardFiles // map (name=>s"$dir$name.indexcard")
      )
    }
  }

  def main(args: Array[String]): Unit = {
    import fm.wrk.grafik.cards.IndexCardProject.{pathName=>cardPathName}

    val dir = args(0)

    val par = Params(dir, args)

    println(s"reading talks from ${par.talksFile}, sessions from ${par.excelIn}, writing ${par.excelOut}, day letters: ${par.dayLetters.mkString(";")}, days: " + par.cardFiles.mkString(", "))

    val rowBase = 8 // for 0-based row increments
    val excelSched = new ExcelSched(par.excelIn, rowBase)

    val dates = (0 to new Period(par.fromDay, par.toDay).getDays) map (par.fromDay.plusDays(_))

    (par.dayLetters zip dates zip par.cardFiles).foldLeft(rowBase) { case (base, ((letter, date), cardFile)) =>
      val cardProject = IndexCardProject(
        name      = cardFile,
        letter    = letter,
        cardsFile = cardPathName(cardFile), // s"${Params.dir}$cardFile.indexcard",
        talksFile = par.talksFile,
        date      = date
      )

      cardProject.schedule.zipWithIndex foreach { case (scheduledTalk, i) =>
        excelSched.talkRow(scheduledTalk, base + i)
      }

      val newBase = base + cardProject.schedule.size
      println(s"base: $newBase")
      newBase
    }
    excelSched.write(par.excelOut)
  }
}
