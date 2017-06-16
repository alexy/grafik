/**
 * Created by akhrabrov on 5/12/15.
 */

package fm.wrk.grafik.note

import com.evernote.auth.{EvernoteAuth, EvernoteService}
import com.evernote.clients.{ClientFactory, NoteStoreClient}
import com.evernote.edam.`type`.{Note, Notebook, Tag}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}
import com.evernote.edam.notestore.NoteFilter
import fm.wrk.grafik.talk.{Summary, Talk}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}


import fm.wrk.util.grafik._

//object `package` {
//
//  implicit class Evernote(s: Summary) {
//
//    val showEmail = tagged(s.email, "<p><pre>")
//    val showCompany = taggedOpt(s.company, "<b>")
//    val showRole = taggedOpt(s.role, "<b><i>")
//    val showOtherTags = if (s.otherTags.isEmpty) "" else s"<p><b>${s.showOtherTags}</b></p>"
//    //  twitter.map(hasPrefix('@')
//
//    override def toString =
//      s"<b>${s.headline}</b><p>$showCompany * $showRole</p>$showOtherTags<p>${s.body}</p>"
//
//    println(toString)
//  }
//}

object N {
  def makeNote(noteStore: NoteStoreClient, noteTitle: String, noteBody: Evernote, parentNotebookOpt: Option[Notebook] = None): Try[Note] = {

    val nBody = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "<!DOCTYPE en-note SYSTEM \"http://xml.evernote.com/pub/enml2.dtd\">" +
      "<en-note>" + noteBody + "</en-note>"

    // Create note object
    val ourNote = new Note()
    ourNote.setTitle(noteTitle)
    ourNote.setContent(nBody)

    // parentNotebook is optional; if omitted, default notebook is used
    for (parentNotebook <- parentNotebookOpt
         if parentNotebook.isSetGuid()) {
      ourNote.setNotebookGuid(parentNotebook.getGuid())
    }

    // Attempt to create note in Evernote account
    try {
      Success(noteStore.createNote(ourNote))
    } catch {
      case edue: EDAMUserException =>
        // Something was wrong with the note data
        // See EDAMErrorCode enumeration for error code explanation
        // http://dev.evernote.com/documentation/reference/Errors.html#Enum_EDAMErrorCode
        println("EDAMUserException: " + edue)
        Failure(edue)
      case ednfe: EDAMNotFoundException =>
        // Parent Notebook GUID doesn't correspond to an actual notebook
        println("EDAMNotFoundException: Invalid parent notebook GUID")
        Failure(ednfe)
      case e: Exception =>
        // Other unexpected exceptions
        e.printStackTrace()
        Failure(e)
    }
  }
}

object Notes {

  val env = "prod" // "dev" or "prod"

  val developerToken: String = readStringMapFromTSV("devtoken.tsv")(env)

  println("devtoken: " + developerToken)

  def main(args: Array[String]): Unit = {

    val talks = Talk.readFromTSV(args(0))

    val group = if (args.size > 1) 1 else 0

    // Set up the NoteStore client
    val evernoteAuth = new EvernoteAuth(EvernoteService.PRODUCTION, developerToken)
    val factory = new ClientFactory(evernoteAuth)
    val noteStore = factory.createNoteStoreClient()

    // Make API calls, passing the developer token as the authenticationToken param
    val notebookList = noteStore.listNotebooks().toList
    //    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

    //    val ourNotebook = noteStore.getDefaultNotebook
    val notebooks = notebookList.map { case nb => (nb.getName, nb) }.toMap
    val ourNotebook = notebooks("sbtb2017")

    val noteFilterOurNotebook = new NoteFilter()
    noteFilterOurNotebook.setNotebookGuid(ourNotebook.getGuid)

    val noteList: List[Note] = noteStore.findNotes(noteFilterOurNotebook, 0, 1000).getNotes.toList

    val allNotes: Map[String, Note] = noteList.map { case note => (note.getTitle, note) }.toMap

    val allTags = noteStore.listTags().toList.map { case t => (t.getName, t) }.toMap
    val ourTags = noteStore.listTagsByNotebook(ourNotebook.getGuid).toList.map { case t => (t.getName, t) }.toMap

    println(s"Tags in the notebook ${ourNotebook.getName}:")
    ourTags.keys foreach { case tag =>
      println("\t" + tag)
    }

    talks
//            .take(1)
      //        .drop(100)
      .foldLeft((allTags, allNotes)) { case ((oldTags, oldNotes), talk) =>
      //      val title = TalkTitle(1, "John Smith")
      //      val body = TalkBody(email = "a@b.com", company = Some("Verizon"), twitter = Some("@jsmith"),
      //        title = "How do you do it?", body = "Sometimes, we just have no idea.", bio = "He was born in a tough part of town")

      val (newTags: Map[String, Tag], noteTags: List[Tag]) = talk.tags.foldLeft((oldTags, Nil: List[Tag])) { case ((allTags, noteTags), tag) =>
        val (nt, t) = allTags.get(tag) match {
          case Some(t) => (allTags, t)
          case _ => // create a new tag
            println(s"creating tag $tag")
            val t = new Tag()
            t.setName(tag)
            noteStore.createTag(t)
            println(s"created new TAG ${t.getName}")
            (allTags + (tag -> t), t)
        }
        (nt, noteTags :+ t)
      }

      val summary = talk.summary
      val headline = summary.headline

      def addTags(note: Note, tags: List[Tag]) = {
        print(s"adding tags to note [${note.getTitle}]: ")
        noteTags foreach { case t =>
          note.addToTagNames(t.getName)
          print("  " + t.getName)
        }
        noteStore.updateNote(note)
        println()
      }

      // TODO instead of deleting the old note outright,
      // we could just update the text, title, and tags
      oldNotes.get(headline) match {
        case Some(n) =>
          noteStore.deleteNote(n.getGuid)
          println(s"deleting previous version of the note [$headline]")
        case _ =>
      }

      N.makeNote(noteStore, headline, summary, Some(ourNotebook)) match {
        case Success(note) =>
          println(s"successfully created note [$headline]")
          addTags(note, noteTags)
          (newTags, oldNotes + (headline -> note))
        case Failure(error) =>
          println(s"FAILED to create a note with TITLE $headline because of $error")
          (oldTags, oldNotes)
      }
    }
  }
}