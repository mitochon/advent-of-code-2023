package aoc

object Day6 extends App {

  case class Race(time: Long, distance: Long) {
    def waysToWin(): Long = {
      (1L until time).map(t => t * (time - t)).count(_ > distance)
    }
    def smallestTimeToWin(): Option[Long] = {
      (1L until time).find(t => t * (time - t) > distance)
    }
    def waysToWinFaster(): Long = {
      smallestTimeToWin() match {
        case Some(t) => time + 1 - (2 * t)
        case None => 0
      }
    }
  }

  val file = getResourceFile("day6/input")
  val lines = getLines(file)
  val times = parseLong(lines.next())
  val distances = parseLong(lines.next())

  // part1
  val races = times.zip(distances).map { case (t, d) => Race(t, d) }
  val wins = races.map(_.waysToWin)

  println(races)
  println(wins)
  println(wins.foldLeft(1L)(_ * _))

  // part2
  val race = Race(times.mkString("").toLong, distances.mkString("").toLong)
  println(race)
  println(race.waysToWinFaster)
}
