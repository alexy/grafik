package fm.wrk.grafik.note

import java.io.{BufferedWriter, File, FileWriter}

object Accepted {

    def main(args: Array[String]): Unit = {

      val talksFile = args(0)
      val acceptedNotebook = args(1)
      val selectedTalksFile = args(2)

      val talkLines = scala.io.Source.fromFile(talksFile).getLines().toList.zipWithIndex.map(_.swap).toMap

      val (noteList, _, _) = ReadNotes.getAllNotebookNotes(acceptedNotebook)

      val noteTitleIdOpt = noteList map { case note =>
        val title = note.getTitle
        val indexOpt =
          try {
            Some(title.split(" ")(0).stripSuffix(":").toInt)
          }
          catch {
            case e: NumberFormatException => None
          }
        (title, indexOpt)
      }

      val noteTitleId = noteTitleIdOpt collect {
        case (title, Some(id)) => (title, id)
      } sortBy(_._2)

      val file = new File(selectedTalksFile)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(talkLines(0)+"\n")

//      noteTitleIdOpt foreach {
//        case (title, Some(index)) =>
//          println(s"[$index] => $title")
//        case (title, _) =>
//            println(s"[manual] => $title")
//      }

      noteTitleId foreach { case (title, id) =>
        println(s"[$id] => $title")
        bw.write(s"${talkLines(id)}\t$id\n")
      }
      bw.close()
    }
}
