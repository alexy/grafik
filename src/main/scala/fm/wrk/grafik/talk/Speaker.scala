package fm.wrk.grafik.talk

/**
 * Created by akhrabrov on 5/28/15.
 */

case class Speaker(
    name:       String,
    email:      String,
    companyOpt: Option[String] = None,
    roleOpt:    Option[String] = None,
    twitterOpt: Option[String] = None,
    bioOpt:     Option[String] = None,
    photoOpt:   Option[String] = None) {

  override def toString: String = {
    List(
      ("name", Some(name)),
      ("email", Some(email)),
      ("company", companyOpt),
      ("role", roleOpt),
      ("photo", photoOpt)
    ).collect{ case (field, Some(text)) => s"$field:\t$text" }
        .mkString("\n")
  }
}
