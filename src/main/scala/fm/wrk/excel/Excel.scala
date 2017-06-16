package fm.wrk.excel

import org.apache.poi.xssf.usermodel.{XSSFCell, XSSFRow}

object Implicits {

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
}