package io.bythebay.sever

/**
 * Created by akhrabrov on 5/28/15.
 */

case class Speaker(
    name: String, email: String,
    companyOpt: Option[String],
    twitterOpt: Option[String],
    bio: String, photoOpt: Option[String])
