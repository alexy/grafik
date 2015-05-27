package io.bythebay

/**
 * Created by a on 5/26/15.
 */
package object sever {
    def so(s: String): Option[String] = Option(s).filter(_.trim.nonEmpty)
    def well(so: Option[String]): String = so.getOrElse("")
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
}
