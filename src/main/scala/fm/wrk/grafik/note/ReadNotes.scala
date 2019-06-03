package fm.wrk.grafik.note

/**
  * Created by alexy on 7/24/18.
  */

import com.evernote.auth.{EvernoteAuth, EvernoteService}
import com.evernote.clients.ClientFactory
import com.evernote.edam.`type`.{Note, Notebook, Tag}
import com.evernote.edam.notestore.NoteFilter
import fm.wrk.util.grafik.readStringMapFromTSV

import scala.collection.JavaConversions._

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

  val allTags = noteStore.listTags().toList.map { case t => (t.getName, t) }.toMap


  def getAllNotebookNotes(ourNotebookName: String): (List[Note], Map[String, Tag], Option[Notebook]) = {
    notebooks.get(ourNotebookName) match {
      case sn@Some(ourNotebook) =>

        val noteFilterOurNotebook = new NoteFilter()
        noteFilterOurNotebook.setNotebookGuid(ourNotebook.getGuid)

        val batchSize = 50 // Evernote fetches only up to 50 notes!

        def fetchPaginated(noteFilter: NoteFilter, batchSize: Int, maxNotesOpt: Option[Int] = Some(1000))(offset: Int, acc: List[Note] = Nil): List[Note] = {
          println(s"fetching notes for offset $offset")
          val chunk = noteStore.findNotes(noteFilterOurNotebook, offset, batchSize).getNotes.toList
          if (chunk.length == 0) acc
          val newAcc = acc ++ chunk
          maxNotesOpt match {
            case Some(maxNotes) if newAcc.length >= maxNotes => newAcc
            case _ =>
              if (chunk.length == batchSize) {
                fetchPaginated(noteFilter, batchSize, maxNotesOpt)(offset + batchSize, newAcc)
              }
              else
                newAcc
          }
        }

        val noteList: List[Note] = fetchPaginated(noteFilterOurNotebook, batchSize)(0)

        println(s"${noteList.length} notes in notebook $ourNotebookName")

        val ourTags = noteStore.listTagsByNotebook(ourNotebook.getGuid).toList.map { case t => (t.getName, t) }.toMap

        (noteList, ourTags, sn)
      case _ => (Nil, Map.empty, None)
    }
  }
}
