package io.bythebay.sever

import java.io._
import java.util.Locale
import org.apache.poi.hssf.usermodel.HSSFDateUtil
import org.apache.poi.ss.util._
import org.apache.poi.xssf.usermodel._

/**
 * Created by a on 5/26/15.
 */
object Excel {
  def main(args: Array[String]): Unit = {
    val days = Array("2015-08-14", "2015-08-15")
    val timetableFile = "timetable.tsv"
    println(s"reading timetable from $timetableFile")
    val timetable: Map[Char,String] = readStringMapFromTSV(timetableFile) map { case (k,v) => (k(0),v) }

    val talksFile     = args(0)
    val sessionsFile  = args(1)
//    val timetableFile = args(2)

    println(s"reading talks from $talksFile, sessions from $sessionsFile")

    val talks = Talk.readFromTSV(talksFile).filter(_.key.nonEmpty).sortBy(_.key)

    println(s"read ${talks.size} keyed talks")

//    sys.exit(0)

    val a0: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap

    val wb = new XSSFWorkbook(
//      getClass.getResourceAsStream("/"+sessionsFile)
      new FileInputStream(sessionsFile)
    )
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

//    // Заполняем шапку таблицы
//    val row = sheet.getRow(9
//    for(idx <- row.getFirstCellNum to row.getLastCellNum) {
//      val cell = row.getCell(idx)
//      val text = if ((3 to 4) contains idx) {
//        //cell.setCellStyle(cellStyle)
//        cell.getDateCellValue()
//      }
//      else
//        cell
//      println(idx + ": " + text)
//    }

    val rowBase = 8
    talks.zipWithIndex foreach { case (talk, talkIndex) =>
      val key = talk.key.get // here the key must be present past filter above
      val talkKey = new TalkKey(key, timetable, days)

      val r = sheet.createRow(rowBase+talkIndex)

      val startFinish = talkKey.startFinish

      // can create an implicit to set cells by letter directly

      implicit class RichXSSFRow(r: XSSFRow) {
        // overloading createCell would silently fall on Char's own implicit conversion to Int
        // and all the cells will be created in 60-90s range...
        def createCellA(c: Char): XSSFCell = {
          val delta =
            if (('a' to 'z') contains c) 'a'
            else if (('A' to 'Z') contains c) 'A'
            else throw new IllegalArgumentException(s"cannot address Excel cells by $c")
          val cellNum = c - delta
          r.createCell(cellNum)
        }
      }

      // r.createCell(a0('a')).setCellValue(key)
      r.createCellA('a').setCellValue(key)
      r.createCellA('b').setCellValue(talk.title)
      r.createCellA('c').setCellValue("Y")

      startFinish.zipWithIndex foreach { case ((day,time), i) =>
        val c = r.createCell(3 + i)
        c.setCellValue(dateFormat.parse(s"$day $time"))
        c.setCellStyle(cellStyleDate1)
      }

      r.createCellA('f').setCellValue(talkKey.track)
      r.createCellA('i').setCellValue(talk.body)
      r.createCellA('j').setCellValue(talk.author)
      r.createCellA('p').setCellValue(talkKey.track)
    }

    val fileOut = new FileOutputStream("workbook.xlsx")
    wb.write(fileOut)
    fileOut.close()

  }
}
