package fm.wrk.grafik

import fm.wrk.grafik.talk.Summary
import fm.wrk.util.grafik._

/**
  * Created by akhrabrov on 7/7/16.
  */
package object note {
  implicit class Evernote(s: Summary) {

    val showTitle     = tagged(s.title, "<h2><b>")
    val showEmail     = tagged(s.email, "<p><pre>")
    val showCompany   = taggedOpt(s.company, "<b>")
    val showRole      = taggedOpt(s.role, "<b><i>")
    val showTwitter   = taggedOpt(s.twitter, "<p><i><pre>")
    val showOtherTags = if (s.otherTags.isEmpty) "" else s"<p><b>${s.showOtherTags}</b></p>"
    //  twitter.map(hasPrefix('@')

    override def toString =
//      s"<b>${s.headline}</b><p>$showCompany * $showRole</p>$showOtherTags<p>${s.body}</p>"
          s"$showTitle<p>$showCompany * $showRole</p>$showEmail$showTwitter<p>${s.body}</p>"

    println(toString)
  }
}
