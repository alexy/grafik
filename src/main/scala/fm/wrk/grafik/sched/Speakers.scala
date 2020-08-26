package fm.wrk.grafik.sched

import java.io.{FileInputStream, FileOutputStream}

import fm.wrk.grafik.talk.Talk
import org.apache.poi.xssf.usermodel.XSSFWorkbook

/**
 * Created by akhrabrov on 5/28/15.
 */
object Speakers {
  def main(args: Array[String]): Unit = {
    import fm.wrk.excel.Implicits._

    val dir          = args(0)

    val files        = args.slice(1,5) map (dir+_)

    val talksFile    = files(0) // NB this is args(1)!
    val acceptedFile = files(1) 
    val excelIn      = files(2)
    // TODO the first empty row can determine automatically
    val excelOut     = files(3)

    val rowBase      = args(5).toInt // 6 from scratch

    val talks    = Talk.readFromTSV(talksFile, acceptedFileOpt = Some(acceptedFile)) //.filter(_.key.nonEmpty).sortBy(_.key)

    val speakers = talks map (_.speaker) sortBy (_.name) distinct

    speakers foreach println

    val wb = new XSSFWorkbook(
      //      getClass.getResourceAsStream("/"+sessionsFile)
      new FileInputStream(excelIn))

    val sheet = wb.getSheetAt(0)

    speakers.zipWithIndex foreach { case (speaker, speakerIndex) =>
      val r = sheet.createRow(rowBase + speakerIndex)
      r.createCellA('a').setCellValue(speaker.name)
      r.createCellA('b').setCellValue(speaker.email)
      r.createCellA('d').setCellValue(speaker.companyOpt.getOrElse(""))
      r.createCellA('e').setCellValue(speaker.roleOpt.getOrElse(""))
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
