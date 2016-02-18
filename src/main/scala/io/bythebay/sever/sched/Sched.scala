package io.bythebay.sever.sched

import java.io._
import java.util.Locale

import io.bythebay.excel.Implicits._
import io.bythebay.sever.TalkKey
import io.bythebay.sever.talk.Talk
import io.bythebay.util.sever._
import org.apache.poi.ss.util._
import org.apache.poi.xssf.usermodel._

/**
 * Created by a on 5/26/15.
 */
object Sched {
  def main(args: Array[String]): Unit = {
    val talksFile     = args(0)
    val dayRange = args.slice(1,3) // Array("2015-08-14", "2015-08-15")
    println(s"filling days ${dayRange(0)}..${dayRange(1)}")

    val excelIn  = args(3)
    val excelOut = args(4)

    val cardFileNames = args.drop(5)

    println(s"reading talks from $talksFile, sessions from $excelIn, writing $excelOut, days: " + cardFileNames.mkString(", "))

    val talks = Talk.readFromTSV(talksFile)

    println(s"read ${talks.size} talks")

    val rowBase = 8 // writing the default Excel template from Sched from this row

//    sys.exit(0)

    val a0: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap

    val wb = new XSSFWorkbook(
//      getClass.getResourceAsStream("/"+sessionsFile)
      new FileInputStream(excelIn)
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

    talks.zipWithIndex foreach { case (talk, talkIndex) =>
      val key = talk.key.get // here the key must be present past filter above

      val r = sheet.createRow(rowBase+talkIndex)

      val startFinish = talkKey.startFinish

      // can create an implicit to set cells by letter directly


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
      r.createCellA('j').setCellValue(talk.speaker.name)
      r.createCellA('p').setCellValue(talkKey.track)
    }

    val fileOut = new FileOutputStream(excelOut)
    wb.write(fileOut)
    fileOut.close()

  }
}
