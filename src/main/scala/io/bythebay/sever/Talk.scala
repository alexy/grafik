package io.bythebay.sever

/**
 * Created by a on 5/26/15.
 */

case class Talk(key: Option[String], id: Int,
                author: String, email: String,
                companyOpt: Option[String],
                twitterOpt: Option[String],
                title: String,
                body: String,
                bio: String)


object Talk {

  def readFromTSV(filename: String): List[Talk] = {
    scala.io.Source.fromFile(filename).getLines().toList match {
      case schemaRow :: lines =>

        val schema: Map[String, Int] = schemaRow.split("\t").zipWithIndex.toMap

        try {
          val key = schema("Key")
          val namePos = schema("Name")
          val emailPos = schema("Email Address")
          val optCompanyPos = schema.get("Company and role") // optional in key, value, field
          val optTwitterPos = schema("Speaker's Twitter handle")
          val titlePos = schema("Title")
          val bodyPos = schema("Abstract")
          val bioPos = schema("Speaker Bio")

          lines.zipWithIndex flatMap { case (line, i) =>
            try {
              val fields: List[String] = line.split("\t").toList.map(xml.Utility.escape)
              val f: Int => String = fieldOrEmpty1(fields)
              val fo: Int => Option[String] = fieldOrNone1(fields)

              val optCompany = for {pos <- optCompanyPos; s <- fo(pos)} yield s

              Some(
                Talk(
                  key=fo(key), id = i, author = f(namePos), email = f(emailPos),
                  companyOpt = optCompany, twitterOpt = fo(optTwitterPos),
                  title = f(titlePos), body = f(bodyPos), bio = f(bioPos)
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
}
