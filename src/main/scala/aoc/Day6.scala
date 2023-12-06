package aoc

object Day6 extends App {

  val numberRegex = """(\d+)""".r

  def parseNumbers(line: String): Seq[Int] = {
    numberRegex.findAllIn(line).map(_.toInt).toSeq
  }

  case class Race(time: Int, distance: Int) {
    def waysToWin(): Int = {
      (1 until time).map(t => t * (time - t))
        .filter(_ > distance)
        .length
    }
  }

  val file = getResourceFile("day6/input")
  val lines = getLines(file)
  val times = parseNumbers(lines.next())
  val distances = parseNumbers(lines.next())

  // part1
  val races = times.zip(distances).map { case (t, d) => Race(t, d) }
  val wins = races.map(_.waysToWin)

  println(races)
  println(wins)
  println(wins.foldLeft(1)(_ * _))

}
