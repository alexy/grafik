package fm.wrk.grafik.talk

import fm.wrk.util.grafik._
import fm.wrk.enex._
import com.lucidchart.open.xtract.{ParseSuccess, XmlReader}
import scala.xml.XML

/**
 * Created by a on 5/26/15.
 */

case class Talk(key:       Option[String],
                id:        Int,
                tags:      List[String],
                tagsOther: List[String],
                title:     String,
                body:      String, // abstract is a keyword in Scala
                speaker:   Speaker,
                summary:   Summary) {
  override def toString: String = List(
    ("id",id),
    ("title", title),
    ("tags", tags.mkString("; ")),
    ("tagsOther", tagsOther.mkString("; ")),
    ("speaker", speaker.toString),
    ("abstract", body)
  ).map{case (field, text) => s"$field:\t$text"}
    .mkString("\n")
}

case class Headline(number: Int, speakerName: String, group: Int = 0) {
  override def toString = (number + group*100).toString + ": " + speakerName
}


case class Summary(
                    email:     String,
                    company:   Option[String],
                    role:      Option[String],
                    headline:  String,
                    title:     String,
                    body:      String,
                    tags:      List[String],
                    otherTags: List[String],
                    twitter:   Option[String],
                    bio:       Option[String]
                  ) {
  val showTags      = tags.mkString(", ")
  val showOtherTags = otherTags.mkString(", ")
  val mayShowOtherTags = if (showOtherTags.isEmpty) "" else s" * $showOtherTags"

  val companyRole = List(company, role) flatMap (identity(_)) mkString " * "

  val companyRole_nl = if (companyRole.isEmpty) "" else companyRole+"\n"

  // synopsis does not include the body
  // for index cards, the body is included separately as text
  val synopsis =
    s"""${title}
       | ---------------------
       | $companyRole_nl $showTags$mayShowOtherTags""".stripMargin
}


object Talk {

// data by the bay 2016
/*
  val keys = Map(
    "timestamp"     -> "Timestamp",
    "name"          -> "Name",
    "company"       -> "Your Company",
    "role"          -> "Role",
    "email"         -> "Email address",
    "tracks"        -> "Which of the Six Conferences is this talk Suitable for?",
    "title"         -> "Talk Title",
    "link1"         -> "Talk Link 1",
    "link2"         -> "Talk Link 2",
    "github"        -> "Talk Github Repo",
    "abstract"      -> "Talk Abstract",
    "code"          -> "How much code will your talk have?",
    "data"          -> "How much data are you going to show?",
    "datasets"      -> "Talk Datasets",
    "datasetsNotes" -> "Talk Datasets, Notes",
    "length"        -> "Talk Duration",
    "notes"         -> "Notes for the organizers",
    "found"         -> "How did you learn about Data By the Bay?",
    "partner"       -> "Would your company be a partner of Data By the Bay?",
    "number"        -> "Manual",
    "keynote"       -> "Keynote"
  )

  val trackTagPrefix = ""
  val trackTags = Map(
    "Text By the Bay" -> "text",
    "Democracy By the Bay" -> "democracy",
    "AIoT By the Bay" -> "aiot",
    "Life Sciences By the Bay" -> "life",
    "Law By the Bay" -> "law",
    "UX By the Bay" -> "ux",
    "General Slot -- 2 hours on general data processing each day" -> "general"
  )
  */

  // Scala By the Bay 2016
  /*
  val keys = Map(
    "timestamp" -> "Timestamp",
    "name" -> "Name",
    "email" -> "Email address",
    "submitter" -> "Submitter's Email (only if separate from speaker's)",
    "twitter" -> "Twitter Handle",
    "twitterCompany" -> "Company Twitter Handle",
    "company" -> "Your Company",
    "role" -> "Role",
    "tracks" -> "Which of the Four Conferences are the best fit?",
    "title" -> "Talk Title",
    "abstract" -> "Talk Abstract",
    "github" -> "Talk Github Repo",
    "datasets" -> "Talk Datasets",
    "link1" -> "Talk Link 1",
    "link1" -> "Talk Link 2",
    "code" -> "How much code will your talk have?",
    "data" -> "How much data are you going to show?",
    "length" -> "Talk Duration",
    "found" ->  "How did you learn about Scala By the Bay?",
    "partner" -> "Would your company be a partner of Data By the Bay?",
    "diversity" -> "Diversity and Community Support",
    "notes" -> "Notes for the organizers",
    "number" -> "Manual",
    "keynote" -> "Keynote"
  )
  val trackTags = Map(
    "Scala By the Bay" -> "scala",
    "Big Data Scala By the Bay" -> "pipelines",
    "Twitter OSS" -> "twitter",
    "Startup Technology" -> "startups"
  )
  */

  // Scale By the Bay 2018
/*  val keys = Map(
  "timestamp" -> "Timestamp",
  "email"     -> "Email Address",
  "name"      -> "Name",
  "submitter" -> "Acceptance Email address",
  "photo"     -> "URL of your photo",
  "twitter"   -> "Twitter Handle",
  "twitterCompany" -> "Company Twitter Handle",
  "company"   -> "Your Company",
  "role"      -> "Role",
  "tracks"    -> "Which of the three tracks is the best fit?",
  "title"     -> "Talk Title",
  "abstract"  -> "Talk Abstract",
  "github"    -> "Talk Github Repo",
  "datasets"  -> "Talk Datasets",
  "link1"     -> "Talk Link 1",
  "link2"     -> "Talk Link 2",
  "code"      -> "How much code will your talk have?",
  "data"      -> "How much data are you going to show?",
  "length"    -> "Talk Duration",
  "found"     -> "How did you learn about Scale By the Bay?",
  "partner"   -> "Would your company be a partner of Scale By the Bay?",
  "diversity" -> "Diversity and Community Support",
  "notes"     -> "Notes for the organizers"
  )
*/

  // SBTB 2019
  // -Timestamp
  // -Email Address
  // -Name
  // -Acceptance Email address
  // -Conference Day Contact Phone
  // -URL of your photo
  // -Twitter Handle
  // -Company Twitter Handle
  // -Have you been a speaker By the Bay?
  // -Which Meetup talks did you give
  // -Your Facebook URL
  // -Your Company Facebook URL
  // -Your LinkedIn URL
  // -Your Company LinkedIn URL
  // -Your Company
  // -Role
  // Bio
  // Optional Co-Presenter Name
  // Optional Co-Presenter Email
  // Optional Co-Presenter Company
  // Optional Co-Presenter Role
  // -Which of the three tracks is the best fit?
  // -Talk Title
  // -Talk Abstract
  // -Talk Github Repo
  // -Talk Datasets
  // -Talk Link 1
  // -Talk Link 2
  // -How much code will your talk have?
  // -How much data are you going to show?
  // -Talk Duration
  // -How did you learn about Scale By the Bay?
  // -Would your company be a partner of Scale By the Bay?
  // -Diversity and Community Support
  // -Notes for the organizers

  // SBTB 2020
  // Timestamp  
  // Email Address 
  // Name  
  // Acceptance Email address  
  // Conference Day Contact Phone  
  // URL of your photo 
  // Twitter Handle  
  // Company Twitter Handle  
  // Have you been a speaker By the Bay? 
  // Which Meetup talks did you give 
  // Your Facebook URL 
  // Your Company Facebook URL 
  // Your LinkedIn URL 
  // Your Company LinkedIn URL 
  // Your Company 
  // Role  
  // Bio 
  // Optional Co-Presenter Name  
  // Optional Co-Presenter Email 
  // Optional Co-Presenter Company 
  // Optional Co-Presenter Role                    
  //          NB: add bio!
  // Which of the three tracks is the best fit?  
  // Talk Title  
  // Talk Abstract 
  // Talk Github Repo  
  // Talk Datasets 
  // Talk Link 1 
  // Talk Link 2 
  // How much code will your talk have?  
  // How much data are you going to show?  
  // Acceptable Talk Duration  
  // Preferred Talk Duration 
  // How did you learn about Scale By the Bay? 
  // Would your company be a partner of Scale By the Bay? 
  // Diversity and Community Support 
  // Notes for the organizers            


  // Scale By the Bay 2019
  val keys = Map(
    "timestamp" -> "Timestamp",
    "email"     -> "Email Address",
    "name"      -> "Name",
    "submitter" -> "Acceptance Email address",
    "phone"     -> "Conference Day Contact Phone",
    "photo"     -> "URL of your photo",
    "twitter"   -> "Twitter Handle",
    "twitterCompany" -> "Company Twitter Handle",
    "wasSpeaker" -> "Have you been a speaker By the Bay?",
    "meetups"   -> "Which Meetup talks did you give",
    "facebook"  -> "Your Facebook URL",
    "facebook"  -> "Your Company Facebook URL",
    "linkedin"  -> "Your LinkedIn URL",
    "linkedin"  -> "Your Company LinkedIn URL",
    "company"   -> "Your Company",
    "role"      -> "Role",
    "bio"       -> "Bio",
    "coName"    -> "Optional Co-Presenter Name",
    "coEmail"   -> "Optional Co-Presenter Email",
    "coCompany" -> "Optional Co-Presenter Company",
    "coRole"    -> "Optional Co-Presenter Role",
    "tracks"    -> "Which of the three tracks is the best fit?",
    "title"     -> "Talk Title",
    "abstract"  -> "Talk Abstract",
    "github"    -> "Talk Github Repo",
    "datasets"  -> "Talk Datasets",
    "link1"     -> "Talk Link 1",
    "link2"     -> "Talk Link 2",
    "code"      -> "How much code will your talk have?",
    "data"      -> "How much data are you going to show?",
    "acceptableLength"  -> "Acceptable Talk Duration",
    "preferredLength"   -> "Preferred Talk Duration",
    "found"     -> "How did you learn about Scale By the Bay?",
    "partner"   -> "Would your company be a partner of Scale By the Bay?",
    "diversity" -> "Diversity and Community Support",
    "notes"     -> "Notes for the organizers"
  )

  val trackTagPrefix = ""
  val trackTags = Map(
    "Functional Programming"                             -> "fp",
    "Reactive/Cloud-Native Applications"                 -> "reactive",
    "End-to-end Data Pipelines (optionally with ML/AI)"  -> "data"
  )


  val acceptableLengthTagPrefix = "a"
  val preferredLengthTagPrefix  = "p"
  val lengthTags = Map(
    "20 minutes" -> "20",
    "30 minutes" -> "30",
    "40 minutes" -> "40",
    "60 minutes" -> "60"
  )

  val dataTagPrefix = "data-"
  val dataTags = Map(
    "Working with public data sets, linked above" -> "linked",
    "Showing proprietary data, lots of it" -> "proprietary",
    "Showing lots of data summaries" -> "summaries",
    "Generally alluding to &quot;data in the cloud&quot;" -> "cloud",
    "It's not a data talk actually, data structures are enough" -> "not"
  )

  val codeTagPrefix = "code-"
  val codeTags = Map(
    "Live coding.  The best!" -> "live",
    "Showing code in an IDE/on Github" -> "github",
    "Code on slides" -> "slides",
    "Making curly braces in the air with hand waves" -> "air"
  )

  val companyTagPrefix = "company-"

  // TODO create sidecar duplicate line file for the main talks.tsv, or mark them in a column
  val duplicates = List[List[Int]]() // List(List(7,8),List(15,128),List(24,25),List(37,38),List(102,117))

  def readFromTSV(filename: String, idPresent: Boolean = false, idBase: Int = 0, dedup: Boolean = false, acceptedFileOpt: Option[String] = None): List[Talk] = {
    val onlyRowsOpt: Option[Seq[Int]] = acceptedFileOpt match { 
      case Some(enexFilename) => 
        println("reading Evernote export from file " +enexFilename+ ", selecting only the talks contained in it")

        val xml = XML.loadFile(enexFilename)
        val parsedEnex = XmlReader.of[Enex].read(xml)

        parsedEnex match {
          case ParseSuccess(s) => // println(s.taggedOnly)
              val RowNumber = "([0-9]+): .*".r
              val ns = s.notes.notes.map(_.title).collect { 
                // println("++" +title)
                _ match { case RowNumber(n) => n }
              } map(_.toInt)
              Some(ns.sorted)
          case _ => None
          }
      case _ => None
      }


    scala.io.Source.fromFile(filename).getLines().toList match {
      case schemaRow :: lines =>

//      println(schemaRow)
        val numberedLines = lines.zipWithIndex
        val lineOffset = 2 // header line and 0-based zipWithIndex
        
        val dropLines: Set[Int] = if (dedup)
            duplicates.map(_.dropRight(1)).reduce(_++_).map(_-lineOffset).toSet
          else Set.empty
        val (dropped, uniques) = numberedLines.partition{ case (_,number) => dropLines.contains(number) }
//        dropped foreach { case (line, number) => println(s"DROP LINE ${number+lineOffset}: $line") }
          println(s"dropped ${dropped.size} duplicate lines")
        val talkLines = onlyRowsOpt match {
          case Some(rows) => 
            // talks are numbered 1-based in Evernote, 0-based in lines 
            val rowSet = rows.map(_-1).toSet
            val (keep, leftOut) = uniques.partition{ case (_,number) => rowSet.contains(number) }
            println(s"kept ${keep.size} talks, left out ${leftOut.size}")
            keep
          case _ => uniques
        } 


        // TODO adding trailing columns for special talks here instead of editing tsv
        // may need to configure

        val schemaKeys = schemaRow.split("\t") // :+ "Manual" :+ "Keynote"
        val schema = schemaKeys.zipWithIndex.toMap

        println("keys:" + schemaKeys.mkString("\n"))

        def position(key: String): Int = schema(keys(key))
        def positionOpt(key: String): Option[Int] = keys.get(key).flatMap(k => schema.get(k))

        try {
          //          val keyPos   = position("key")
          val namePos    = position("name"); println("namePos:" + namePos)
          val emailPos   = position("email"); println("emailPos:" + emailPos)

          //          val optCompanyPos = tryKeys(schema)(List("Company and role", "Current company and role")) // optional in key, value, field
          val companyPos = position("company"); println("companyPos:" + companyPos)
          val rolePos    = position("role"); println("rolePos:" + rolePos)
          val photoPos   = position("photo"); println("photoPos:" + photoPos)

          val twitterPos = position("twitter"); println("twitterPos:" + twitterPos)

          val titlePos   = position("title"); println("titlePos:" + titlePos)
          val bodyPos    = position("abstract"); println("bodyPos:" + bodyPos)
          val acceptableLengthPos = position("acceptableLength"); println("acceptableLengthPos:" + acceptableLengthPos)
          val preferredLengthPos  = position("preferredLength");  println("preferredLengthPos:"  + preferredLengthPos)

          //          val optTwitterPos = schema("Speaker's Twitter handle")
          //          val bioPos        = schema("Speaker Bio")
          //          val optPhotoPos   = schema("Speaker Photo")

          val tracksPos  = position("tracks"); println("tracksPos:" + tracksPos)
          val dataPos    = position("data"); println("dataPos:" + dataPos)
          val codePos    = position("code"); println("codePos:" + codePos)
          val numberPosOpt  = positionOpt("number");
          val keynotePosOpt = positionOpt("keynote")

          talkLines flatMap { case (line, i) =>
//            println(line); System.out.flush()
            try {
              val fields: List[String] = line.split("\t").toList.map(xml.Utility.escape)
              val f:  Int => String         = fieldOrEmpty1(fields)
              val fo: Int => Option[String] = fieldOrNone1(fields)
              val foo: Option[Int] => Option[String] = _ match {
                case Some(i) => fo(i)
                case _ => None
              }

              //              val optCompany = for {pos <- optCompanyPos; s <- fo(pos)} yield s
              val companyOpt = fo(companyPos)

              val speaker =
                Speaker(
                  name       = f(namePos),
                  email      = f(emailPos),
                  companyOpt = companyOpt,
                  roleOpt    = fo(rolePos),
                  photoOpt   = fo(photoPos),
                  twitterOpt = fo(twitterPos).map { s =>
                    s.toList match {
                      case '@' :: _ => s
                      case  Nil => s
                      case  _  => '@' + s
                    }
                  } //, bio = f(bioPos), photoOpt = fo(optPhotoPos)
                )

              val keynoteOpt = foo(keynotePosOpt)

              val (tags, tagsOther) = {
                val (t, ta) = resolveTags(trackTags,  trackTagPrefix)(f(tracksPos))
//                println(s"tracks: " + t)
                val (a, aa) = resolveTags(lengthTags, acceptableLengthTagPrefix)(f(acceptableLengthPos))
                val (p, pa) = resolveTags(lengthTags, preferredLengthTagPrefix)(f(preferredLengthPos))
                val (d, da) = resolveTags(dataTags,   dataTagPrefix)(f(dataPos))
                val (c, ca) = resolveTags(codeTags,   codeTagPrefix)(f(codePos))

                val companyTag = companyOpt.map {
                  case c => val normC = defangTag(c).toLowerCase
                  s"$companyTagPrefix$normC"
                }

                (t ++ a ++ p ++ d ++ c ++ companyTag.toList ++ keynoteOpt.toList,
                  List(ta, aa, pa, da, ca).flatten)
              }

              val number = foo(numberPosOpt).map{ x =>
                val n = x.toInt
                println(s"MANUAL number: $n")
                n } getOrElse {
                if (idPresent) {
                  val n = fields.last.toInt
                  println(s"SUFFIX id: $n")
                  n
                }
                else
                  idBase + i + 1
              }
              val title = f(titlePos)
              val body  = f(bodyPos)

             println(number + " => " + title)

              val headline = Headline(number, speaker.name)

              val summary =
                Summary(
                  email     =speaker.email,
                  twitter   =speaker.twitterOpt,
                  company   =speaker.companyOpt,
                  role      =speaker.roleOpt,
                  bio       =speaker.bioOpt,
                  headline  =headline.toString,
                  tags      =tags,
                  otherTags =tagsOther,
                  title     =title,
                  body      =body
                )

              Some(
                Talk(
                  key       = None, //fo(key),
                  id        = number,
                  tags      = tags,
                  tagsOther = tagsOther,
                  title     = title,
                  body      = body,
                  speaker   = speaker,
                  summary   = summary
                )
              )
            } catch {
              case e: NoSuchElementException =>
                println("missing element: " + e)
                None
            }
          }
        } catch {
          case e: NoSuchElementException =>
            println("missing field: " + e)
            List() // or we can simply return a Try
        }

      case _ => println("there seems to be not enough data in your table!")
        List()
    }
  }

  def talkDays(dayTags: List[String], talks: List[Talk]): Map[String, List[Talk]] = {
    val (days, rest) = dayTags.foldLeft((Map[String, List[Talk]](), talks)) {
      case ((days, talks), dayTag) =>
        val (day, rest) = talks.partition(_.tags.contains(dayTag))
      (days + (dayTag->day), rest)
    }
    if (rest.nonEmpty) {
      val sinkTag = dayTags.last
      println(s"These talks are MISSING TAGS: they didn't end up in any of the days, adding to $sinkTag:")
      rest.foreach { case talk => println(talk.summary.headline) }
      days + (sinkTag -> (days(sinkTag) ++ rest))
    } else
      days
  }
}

object ShowTalks {

  def main(args: Array[String]): Unit = {
    val inputFile = if (args.length>0) args(0) else "dbtb.tsv"
    println("showing talks from " + inputFile)

    val acceptedEnexFile = if (args.length>1) {
      val enexFilename = args(1)
      println("keeping only talks from Evernote notebook export file " + enexFilename)
      Some(enexFilename) 
    }else None

    val talks = Talk.readFromTSV(inputFile, acceptedFileOpt = acceptedEnexFile)

    talks foreach { case t =>
        // val tags = t.tags.mkString(";")
        // val tagsOther = t.tagsOther.mkString(";")
        // println(s"tags: $tags ... other: $tagsOther")
        println(s"${t.id}: ${t.title}")
    }
  }
}
