/**
 * Created by akhrabrov on 5/12/15.
 */

package io.bythebay.sever

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

import com.evernote.auth.{EvernoteService, EvernoteAuth}
import com.evernote.clients.{ClientFactory, NoteStoreClient}
import com.evernote.edam.`type`.{Note, Notebook}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}




case class TalkTitle(number: Int, speakerName: String, group: Int = 0) {
  override def toString = (number + group*100).toString + ": " + speakerName
}

case class TalkBody(email: String, company: Option[String],  twitter: Option[String], title: String, body: String, bio: String) {
  override def toString =
  s"<p><pre>$email</pre></p><p>${well(company)}</p>" +
  s"<p>${well(twitter.map(hasPrefix('@')))}</p><br/><p><b>$title</b></p><br/><p>$body</p><br/><hr/><br/><p>$bio</p>"
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
    val notebooks = noteStore.listNotebooks().toList

    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

    talks foreach { case talk =>
//      val title = TalkTitle(1, "John Smith")
//      val body = TalkBody(email = "a@b.com", company = Some("Verizon"), twitter = Some("@jsmith"),
//        title = "How do you do it?", body = "Sometimes, we just have no idea.", bio = "He was born in a tough part of town")

      val speaker = talk.speaker
      val title = TalkTitle(talk.id+2, speaker.name, group)
      val body = TalkBody(email=speaker.email,
        twitter=speaker.twitterOpt, company=speaker.companyOpt,
        title=talk.title, body=talk.body, bio=speaker.bio)

      println("title: " + title)
      println("body:  " + body)
      //N.makeNote(noteStore, title.toString, body.toString)
    }
  }
}
