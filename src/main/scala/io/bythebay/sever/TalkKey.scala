package io.bythebay.sever

/**
 * Created by akhrabrov on 5/27/15.
 */
class TalkKey(key: String, timetable: Map[Char, String], days: Array[String]) {
  assert(key.size>=2,"talk key must be at least two characters long")
  val tracks: Map[Char, String] = Map('I'->"Track A", 'J'->"Track B",'A'->"Keynote")


  val timeChar:  Char = key(0)
  val trackChar: Char = key(1)

  def dayDelta = 11
  def dayShift(c: Char): Char = (c.toInt-dayDelta).toChar
  def times(range: String): Array[String] = range.split("-")
  val (day,range) = timetable.get(timeChar) match {
    case Some(range) => (days(0), range)
    case _ => (days(1), timetable(dayShift(timeChar)))
  }

  val startFinish = Array(day, day) zip times(range)
  val track = tracks.get(trackChar).getOrElse(tracks('A'))
}
