package io.bythebay

/**
  * Created by a on 2/10/16.
  */
package object util {
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
            (tagString: String): List[String] = {
    val candidates = tagString.split(", ")
    candidates map { case x =>
        dict.get(x).getOrElse(default)
    } toList
  }
}
