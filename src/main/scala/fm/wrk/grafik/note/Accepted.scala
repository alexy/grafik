package fm.wrk.grafik.note

import fm.wrk.util.grafik.readStringMapFromTSV
import com.evernote.auth.{EvernoteAuth, EvernoteService}
import com.evernote.clients.ClientFactory
import com.evernote.edam.`type`.{Note, Notebook, Tag}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}
import com.evernote.edam.notestore.NoteFilter
import fm.wrk.grafik.talk.{Summary, Talk}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import java.io.{BufferedWriter, File, FileWriter}


/**
  * Created by alexy on 7/24/18.
  */

object ReadNotes {

  val env = "prod" // "dev" or "prod"

  val developerToken: String = readStringMapFromTSV("devtoken.tsv")(env)

  println("devtoken: " + developerToken)


  // Set up the NoteStore client
  val evernoteAuth = new EvernoteAuth(EvernoteService.PRODUCTION, developerToken)
  val factory = new ClientFactory(evernoteAuth)
  val noteStore = factory.createNoteStoreClient()

  // Make API calls, passing the developer token as the authenticationToken param
  val notebookList = noteStore.listNotebooks().toList
  //    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

  //    val ourNotebook = noteStore.getDefaultNotebook
  val notebooks = notebookList.map { case nb => (nb.getName, nb) }.toMap

  def getAllNotebookNotes(ourNotebookName: String): List[Note] = {
    val ourNotebook = notebooks(ourNotebookName)

    val noteFilterOurNotebook = new NoteFilter()
    noteFilterOurNotebook.setNotebookGuid(ourNotebook.getGuid)

    val batchSize = 50 // Evernote fetches only up to 50 notes!

    def fetchPaginated(noteFilter: NoteFilter, batchSize: Int, maxNotes: Int = 100)(offset: Int, acc: List[Note] = Nil): List[Note] = {
      println(s"fetching notes for offset $offset")
      val chunk = noteStore.findNotes(noteFilterOurNotebook, offset, batchSize).getNotes.toList
      if (chunk.length == 0) acc
      val newAcc = acc ++ chunk
      if (newAcc.length >= maxNotes) newAcc
      if (chunk.length == batchSize) {
        fetchPaginated(noteFilter, maxNotes)(offset + batchSize, newAcc)
      }
      else
        newAcc
    }

    val noteList: List[Note] = fetchPaginated(noteFilterOurNotebook, batchSize)(0)

    println(s"${noteList.length} notes in notebook $ourNotebookName")

    noteList
  }

}
object Accepted {

    def main(args: Array[String]): Unit = {

      val talksFile = args(0)
      val acceptedNotebook = args(1)
      val selectedTalksFile = args(2)

      val talkLines = scala.io.Source.fromFile(talksFile).getLines().toList.zipWithIndex.map(_.swap).toMap

      val noteList = ReadNotes.getAllNotebookNotes(acceptedNotebook)

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
        bw.write(s"$id: ${talkLines(id)}\n")
      }
      bw.close()
    }
}
