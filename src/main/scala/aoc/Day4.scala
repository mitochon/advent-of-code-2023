package aoc

object Day4 extends App {

  private val cardRegex = """Card\s+(\d+): (.+) \| (.+)""".r

  case class Card(id: Int, winners: Seq[Int], numbers: Seq[Int]) {

    def matchingNumbers(): Int = {
      winners.toSet.intersect(numbers.toSet).size
    }
    def points(): Int = {
      matchingNumbers() match {
        case 0 => 0
        case i => List.fill(i - 1)(2).foldLeft(1)(_ * _)
      }
    }
  }

  object Card {
    def apply(line: String): Card = {
      def toNumbers(line: String) = line.split("\\s").filter(_.nonEmpty).map(_.trim.toInt)
      line match {
        case cardRegex(id, w, h) => Card(id.toInt, toNumbers(w), toNumbers(h))
        case _ => throw new IllegalArgumentException(s"Invalid line: $line")
      }
    }
  }

  val file = getResourceFile("day4/input")

  // part1
  val cards = getLines(file).map(Card(_)).toList
  println(cards.map(_.points()).sum)
}
