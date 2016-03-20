package io.bythebay.util

/**
  * Created by a on 2/14/16.
  */
package object sever {
    val digits = """\D*(\d+).*""".r
    def firstInt(s: String): Option[Int] = digits.unapplySeq(s) match {
        case Some(id::_) => Some(id.toInt)
        case _ => None
    }

    def so(s: String): Option[String] = Option(s).filter(_.trim.nonEmpty)

    implicit class RichOptionString(val so: Option[String]) {
        def show: String = so.getOrElse("")
    }

    def showMaybe(so: Option[String], prefix: String = "", suffix: String = ""): String = so.map(x => s"$prefix$x$suffix").getOrElse("")

    def tagged(s: String, prefix: String): String = {
        // prefix.replaceAll("<","</") will not work for nested tags!
        val suffix = prefix.split("<").filter(_.nonEmpty).map("</"+_).reverse.mkString
        s"$prefix$s$suffix"
    }
    def taggedOpt(so: Option[String], prefix: String = ""): String = so.map(tagged(_,prefix)).getOrElse("")
    def hasPrefix(p: Char)(s: String): String = if(s(0) == p) s else p +: s
    def fieldOr[T](e: T)(f: String => T)(l: List[String])(n: Int): T =
      try { f(l(n)) } catch { case _: IndexOutOfBoundsException => e }

    def fieldOrEmpty(l: List[String])(n: Int):  String         = try { l(n) }     catch { case _: IndexOutOfBoundsException => "" }
    def fieldOrNone(l: List[String])(n: Int):   Option[String] = try { so(l(n)) } catch { case _: IndexOutOfBoundsException => None}

    def fieldOrEmpty1(l: List[String])(n: Int) = fieldOr("")(identity[String])(l)(n)
    def fieldOrNone1(l: List[String])(n: Int)  = fieldOr(None: Option[String])((x:String)=>so(x))(l)(n)

    def readStringMapFromTSV(filename: String, separator: String = "\t") = scala.io.Source.fromFile(filename).getLines()
      .map(_.split(separator)).toList
      .foldLeft(Map[String, String]()) { case (m, a) => m + (a(0) -> a(1))}

    def tryKeys[K,V](m: Map[K,V])(keys: List[K]): Option[V] = keys match {
        case key :: rest => m.get(key) match {
            case res @ Some(_) => res
            case _ => tryKeys(m)(rest)
        }
        case _ => None
    }

    def resolveTags(dict: Map[String, String],
                    prefix: String = "",
                    default: String = "other")
                   (tagString: String): (List[String], Option[String]) = {
        val tags = dict.foldLeft(Nil: List[String]) { case (acc, (k, v)) =>
            if (tagString.contains(k)) acc :+ s"$prefix$v" else acc
        }
        if (tags.isEmpty) (List(s"$prefix$default"), Some(tagString)) else (tags, None)
    }
}
