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
            (tagString: String): (List[String], Option[String]) = {
      val tags = dict.foldLeft(Nil: List[String]) { case (acc, (k, v)) =>
          if (tagString.contains(k)) acc :+ s"$prefix$v" else acc
      }
      if (tags.isEmpty) (List(s"$prefix$default"), Some(tagString)) else (tags, None)
    }
}
