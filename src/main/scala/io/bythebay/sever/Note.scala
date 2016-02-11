/**
 * Created by akhrabrov on 5/12/15.
 */

package io.bythebay.sever


import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

import com.evernote.auth.{EvernoteService, EvernoteAuth}
import com.evernote.clients.{ClientFactory, NoteStoreClient}
import com.evernote.edam.`type`.{Tag, Note, Notebook}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}
import com.evernote.edam.notestore.NoteFilter

case class TalkTitle(number: Int, speakerName: String, group: Int = 0) {
  override def toString = (number + group*100).toString + ": " + speakerName
}

case class TalkBody(
                     email:     String,
                     company:   Option[String],
                     role:      Option[String],
                     body:      String,
                     tags:      List[String],
                     tagsOther: List[String],
                     twitter:   Option[String],
                     bio:       Option[String]
                   ) {
  def show(ts: List[String]): String = ts.mkString(", ")

  val showEmail     = tagged(email,      "<p><pre>")
  val showCompany   = taggedOpt(company, "<b>")
  val showRole      = taggedOpt(role,    "<b><i>")
  val showTagsOther = if (tagsOther.isEmpty) "" else "<br/><p>other tags: <b>${show(tagsOther)}</b></p>"
  //  twitter.map(hasPrefix('@')

  override def toString =
  s"$showEmail<p>$showCompany * $showRole</p><br/>$showTagsOther<p>$body</p>"

  println(toString)
}


object N {
  def makeNote(noteStore: NoteStoreClient, noteTitle: String, noteBody: String, parentNotebookOpt: Option[Notebook] = None): Try[Note] = {

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

object Main {

  val env = "prod" // "dev" or "prod"

  val developerToken: String = readStringMapFromTSV("devtoken.tsv"," ")(env)

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
    val notebooks = notebookList.map{ case nb => (nb.getName, nb) }.toMap
    val ourNotebook = notebooks("DBTB")

    val noteFilterOurNotebook = new NoteFilter()
    noteFilterOurNotebook.setNotebookGuid(ourNotebook.getGuid)

    val noteList: List[Note] = noteStore.findNotes(noteFilterOurNotebook,0,1000).getNotes.toList

    val allNotes: Map[String, Note] = noteList.map{ case note => (note.getTitle, note)}.toMap

    val allTags = noteStore.listTags().toList.map { case t => (t.getName, t) }.toMap
    val ourTags = noteStore.listTagsByNotebook(ourNotebook.getGuid).toList.map { case t => (t.getName, t) }.toMap

    println(s"Tags in the notebook ${ourNotebook.getName}:")
    ourTags.keys foreach { case tag =>
      println("\t"+tag)
    }

    talks
//      .take(1)
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
              (allTags + (tag->t), t)
          }
        (nt, noteTags :+ t)
      }

      val speaker   = talk.speaker
      val talkTitle = TalkTitle(talk.id+1, speaker.name, group)
      val title     = talkTitle.toString

      val body  = TalkBody(
        email     =speaker.email,
        twitter   =speaker.twitterOpt,
        company   =speaker.companyOpt,
        role      =speaker.roleOpt,
        tags      =talk.tags,
        tagsOther =talk.tagsOther,
        body      =talk.body,
        bio       =speaker.bioOpt)

      def addTags(note: Note, tags: List[Tag]) = {
        print(s"adding tags to note [${note.getTitle}]: ")
        noteTags foreach { case t =>
          note.addToTagNames(t.getName)
          print("  "+t.getName)
        }
        noteStore.updateNote(note)
        println()
      }

      // TODO instead of deleting the old note outright,
      // we could just update the text, title, and tags
      oldNotes.get(title) match {
        case Some(n) =>
          noteStore.deleteNote(n.getGuid)
        case _ =>
      }

      N.makeNote(noteStore, title, body.toString) match {
            case Success(note) =>
              println("successfully cteated note with title: " + title)
              addTags(note, noteTags)
              (newTags, oldNotes + (title->note))
            case Failure(error) =>
              println(s"FAILED to create a note with TITLE $title because of $error")
              (oldTags, oldNotes)
      }
    }
  }
}
