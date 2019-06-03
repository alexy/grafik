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

  import ReadNotes.{noteStore, allTags}

  def main(args: Array[String]): Unit = {

    val ourNotebookName = args(1)

    val dry = false
    val dryShow = if (dry) "DRY " else ""
    val onlyNewTalks = true

    val talks = Talk.readFromTSV(args(0))

    val group = if (args.size > 1) 1 else 0

    val (noteList, ourTags, ourNotebookOpt): (List[Note], Map[String, Tag], Option[Notebook])=
      ReadNotes.getAllNotebookNotes(ourNotebookName)

    val allNotes: Map[String, Note] = noteList.map { case note => (note.getTitle, note) }.toMap

    allNotes foreach { case (title, note) =>
        println(title)
    }


    println(s"Tags in the notebook ${ourNotebookName}:")
    ourTags.keys foreach { case tag =>
      println("\t" + tag)
    }

    // DANGER!  use this to clear all tags in the notebook!
//    noteStore.listTagsByNotebook(ourNotebook.getGuid).toList.map { case t =>
//        println(s"expunging tag ${t.getName}")
//        DONT! noteStore.expungeTag(t.getGuid())
//    }

    talks
//            .take(3)
      //        .drop(100)
      .foldLeft((allTags, allNotes)) { case ((oldTags, oldNotes), talk) =>
      //      val title = TalkTitle(1, "John Smith")
      //      val body = TalkBody(email = "a@b.com", company = Some("Verizon"), twitter = Some("@jsmith"),
      //        title = "How do you do it?", body = "Sometimes, we just have no idea.", bio = "He was born in a tough part of town")

      val (newTags: Map[String, Tag], noteTags: List[Tag]) = talk.tags.foldLeft((oldTags, Nil: List[Tag])) { case ((allTags, noteTags), tag) =>
        val (nt, t) = allTags.get(tag) match {
          case Some(t) => (allTags, t)
          case _ => // create a new tag
            println(s"$dryShow creating tag $tag")
            val t = new Tag()
            t.setName(tag)
            if (!dry) noteStore.createTag(t)
            println(s"$dryShow created new TAG ${t.getName}")
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
        if (!dry) noteStore.updateNote(note)
        println()
      }

      // TODO instead of deleting the old note outright,
      // we could just update the text, title, and tags

      val prevNoteOpt = oldNotes.get(headline)
      prevNoteOpt match {
        case Some(n) if onlyNewTalks =>
          println(s"not a new talk, skipping [$headline]")
          (oldTags, oldNotes)
        case _ =>
//          prevNoteOpt match {
//            case Some(n) =>
//              if (!dry) noteStore.deleteNote(n.getGuid)
//              println(s"$dryShow deleting previous version of the note [$headline]")
//            case _ =>
//          }

          if (talk.id <= 142) {
            println(s"skipping low-numbered talk [$headline]")
            (oldTags, oldNotes)
          }
          else {

            println(s"$dryShow creating note [$headline]")
            if (dry)
              (oldTags, oldNotes)
            else {
              N.makeNote(noteStore, headline, summary, ourNotebookOpt) match {
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
    }
  }
}
