package fm.wrk.grafik.show

/**
  * Created by alexy on 6/30/17.
  */

import java.io.PrintWriter

import fm.wrk.grafik.talk.Talk

object Speakers {
  def main(args: Array[String]): Unit = {

    val talksFile = args(0)
    val emailFile = args(1)
    val filterOut = if (args.length>=3) Some(args(2)) else None

    val talks = Talk.readFromTSV(talksFile) //.filter(_.key.nonEmpty).sortBy(_.key)

    val speakers = talks map (_.speaker) sortBy (_.name) distinct

    val fileOut = new PrintWriter(emailFile)

    speakers foreach { case speaker =>
      speaker.companyOpt match {
        case Some(company) if filterOut.nonEmpty && company.contains(filterOut.get) => println(s"skipping: ${speaker.name} -- ${filterOut.get}")
        case _ => fileOut.write(s"${speaker.name} <${speaker.email}>,\n")
      }
    }

    fileOut.close()
  }
}