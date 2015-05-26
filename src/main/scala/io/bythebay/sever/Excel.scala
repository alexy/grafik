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
    val timetableFile = "timetable.tsv"

//    val timetable =readStringMapFromTSV(timetableFile)

    val talksFile     = args(0)
    val sessionsFile  = args(1)
//    val timetableFile = args(2)

    println(s"reading talks from $talksFile, sessions from $sessionsFile")

    val talks = Talk.readFromTSV(talksFile)

    println(s"read ${talks.size} talks")

    val a0: Map[Char, Int] = ('a' to 'z').zipWithIndex.toMap

    val wb = new XSSFWorkbook(
//      getClass.getResourceAsStream("/"+sessionsFile)
      new FileInputStream(sessionsFile)
    )
    val sheet = wb.getSheetAt(0)

    val javaDateFormat = "yyyy-MM-dd HH:mm"
    val dateFormat = new java.text.SimpleDateFormat(javaDateFormat)

    val excelFormatPattern = DateFormatConverter.convert(Locale.US,javaDateFormat)

    val cellStyleDate1 = wb.createCellStyle()
    val poiFormat = wb.createDataFormat()
    cellStyleDate1.setDataFormat(poiFormat.getFormat(excelFormatPattern))

    val createHelper = wb.getCreationHelper()
    val cellStyleDate2 = wb.createCellStyle()
    cellStyleDate2.setDataFormat(
      createHelper.createDataFormat().getFormat("m/d/yy h:mm"))

    // Заполняем шапку таблицы
    val row = sheet.getRow(9)
    for(idx <- row.getFirstCellNum to row.getLastCellNum) {
      val cell = row.getCell(idx)
      val text = if ((3 to 4) contains idx) {
        //cell.setCellStyle(cellStyle)
        cell.getDateCellValue()
      }
      else
        cell
      println(idx + ": " + text)
    }


    /*
    0: 2
    1: Keynote II: How important is choice of language to build a scalable platform?
      2: Y
    3: Fri Aug 14 09:40:00 PDT 2015
    4: Fri Aug 14 10:10:00 PDT 2015
    5:
    6:
    7:
    8: We all know technology choices
    9: Vidhya Narayanan
    10:
    11:
    12:
    13:
    14:
    15: TBA
    */

    val r  = sheet.createRow(12)
    r.createCell(0).setCellValue("SEI5")
    r.createCell(1).setCellValue("How to catch a Kangaroo")
    r.createCell(2).setCellValue("Y")

    val startEnd = List("10:10", "10:50")
    val day = "2015-08-14"

    startEnd.zipWithIndex foreach { case (time,i) =>
      val c = r.createCell(3+i)
      c.setCellValue(dateFormat.parse(s"$day $time"))
      c.setCellStyle(cellStyleDate1)
    }

    r.createCell(a0('f')).setCellValue("Track A")
    r.createCell(a0('i')).setCellValue("Catching Kangaroos is Hard but Rewarding")
    r.createCell(a0('j')).setCellValue("John Smith")
    r.createCell(a0('p')).setCellValue("Track A")

    val fileOut = new FileOutputStream("workbook.xlsx")
    wb.write(fileOut)
    fileOut.close()

  }
}
