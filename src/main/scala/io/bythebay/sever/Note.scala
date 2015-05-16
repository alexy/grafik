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


object Util {
  def so(s: String): Option[String] = Option(s).filter(_.trim.nonEmpty)
  def well(so: Option[String]): String = so.getOrElse("")
  def hasPrefix(p: Char)(s: String): String = if(s(0) == p) s else p +: s
  def fieldOr[T](e: T)(f: String => T)(l: List[String])(n: Int): T =
    try { f(l(n)) } catch { case _: IndexOutOfBoundsException => e }

//  def fieldOrEmpty1(l: List[String])(n: Int): String         = fieldOr("")(identity[String])(_)(_)
//  def fieldOrNone1(l: List[String])(n: Int):  Option[String] = fieldOr(None: Option[String])((x:String) => Option(x))
  def fieldOrEmpty(l: List[String])(n: Int):  String         = try { l(n) }     catch { case _: IndexOutOfBoundsException => "" }
  def fieldOrNone(l: List[String])(n: Int):   Option[String] = try { so(l(n)) } catch { case _: IndexOutOfBoundsException => None}
}
import Util._

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

  val developerToken = "<mytoken>"

  def main(args: Array[String]): Unit = {

    val group = if (args.size > 1) 1 else 0

    // Set up the NoteStore client
    val evernoteAuth = new EvernoteAuth(EvernoteService.PRODUCTION, developerToken)
    val factory = new ClientFactory(evernoteAuth)
    val noteStore = factory.createNoteStoreClient()

    // Make API calls, passing the developer token as the authenticationToken param
    val notebooks = noteStore.listNotebooks().toList

    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

    val title = TalkTitle(1, "Abram Katz")
    val body = TalkBody(email = "a@b.com", company = Some("Verizon"), twitter = Some("@abrasha"),
      title = "How do you do it?", body = "Sometimes, we just have no idea.", bio = "He was born in a workers part of town")


    scala.io.Source.fromFile(args(0)).getLines().toList match {
      case schemaRow :: lines =>

        val schema: Map[String, Int] = schemaRow.split("\t").zipWithIndex.toMap

        try {
          val namePos       = schema("Name")
          val emailPos      = schema("Email Address")
          val optCompanyPos = schema.get ("Company and role") // optional in key, value, field
          val optTwitterPos = schema("Speaker's Twitter handle")
          val titlePos      = schema("Title")
          val bodyPos       = schema("Abstract")
          val bioPos        = schema("Speaker Bio")

          lines.zipWithIndex foreach { case (line, i) =>
            val fields: List[String] = line.split("\t").toList.map(xml.Utility.escape)
            val f: Int => String = fieldOrEmpty(fields)
            val fo: Int => Option[String] = fieldOrNone(fields)

            val title = TalkTitle(i + 2, f(namePos), group)
            val body = TalkBody(f(emailPos), optCompanyPos.map(f(_)), fo(optTwitterPos), f(titlePos), f(bodyPos), f(bioPos))

            println("title: " + title)
            println("body:  " + body)
            N.makeNote(noteStore, title.toString, body.toString)
          }
      } catch {
          case e: NoSuchElementException =>
            println("missing field: " + e)
        }

      case _ => println("there seems to be not enough data in your table!")
    }

  }
}
