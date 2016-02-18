package io.bythebay.sever.talk

import java.io.{FileInputStream, FileOutputStream}

import io.bythebay.excel.Implicits._
import org.apache.poi.xssf.usermodel.XSSFWorkbook

/**
 * Created by akhrabrov on 5/28/15.
 */
object Speakers {
  def main(args: Array[String]): Unit = {
    val talksFile = args(0)

    val excelIn  = args(1)
    // TODO the first empty row can determine automatically
    val rowBase  = args(2).toInt // 6 from scratch
    val excelOut = args(3)

    val talks = Talk.readFromTSV(talksFile)//.filter(_.key.nonEmpty).sortBy(_.key)

    val speakers = talks map (_.speaker) sortBy (_.name)

    speakers foreach println

    val wb = new XSSFWorkbook(
      //      getClass.getResourceAsStream("/"+sessionsFile)
      new FileInputStream(excelIn))

    val sheet = wb.getSheetAt(0)

    speakers.zipWithIndex foreach { case (speaker, speakerIndex) =>
      val r = sheet.createRow(rowBase + speakerIndex)
      r.createCellA('a').setCellValue(speaker.name)
      r.createCellA('b').setCellValue(speaker.email)
      r.createCellA('g').setCellValue(speaker.bioOpt.getOrElse(""))
      speaker.twitterOpt foreach { handle =>
        r.createCellA('h').setCellValue("twitter.com/" + handle.substring(1, handle.size))
      }
      speaker.photoOpt foreach { photo =>
        r.createCellA('i').setCellValue(photo)
      }

    }

    val fileOut = new FileOutputStream(excelOut)
    wb.write(fileOut)
    fileOut.close()

  }
}
