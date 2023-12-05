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
  val cards = getLines(file).map(Card(_)).toList

  // part1
  println(cards.map(_.points()).sum)

  // part2
  def addCopies(tally: Map[Int, Int], id: Int, matches: Int): Map[Int, Int] = {
    (id + 1 to id + matches).foldLeft(tally) {
      case (acc, i) => acc + (i -> (tally.getOrElse(id, 1) + acc.getOrElse(i, 0)))
    }
  }

  // initial tally is 1 for each card
  val initialTally = cards.map(_.id).zip(List.fill(cards.size)(1)).toMap
  val withCopies = cards.map(c => (c.id, c.matchingNumbers())).foldLeft(initialTally) {
    case (tally, (id, num)) => addCopies(tally, id, num)
  }
  // debug
  withCopies.toList.sortBy(_._1).foreach(println)
  println(withCopies.map(_._2).sum)
}
