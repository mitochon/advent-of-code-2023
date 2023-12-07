package aoc

object Day7 extends App {

  val labels = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse
  val labelPoints = labels.zipWithIndex.toMap

  case class CamelCard(hand: String, bid: Int) extends Ordered[CamelCard] {

    def getKind(): Int = {
      hand.groupBy(identity).view
        .mapValues(_.size)
        .values.toList
        .sorted.reverse match {
          case 5 :: xs => 6
          case 4 :: xs => 5
          case 3 :: 2 :: xs => 4
          case 3 :: xs => 3
          case 2 :: 2 :: xs => 2
          case 2 :: xs => 1
          case _ => 0
        }
    }

    def isStronger(other: CamelCard): Boolean = {
      val points = hand.flatMap(labelPoints.get)
      val otherPoints = other.hand.flatMap(labelPoints.get)
      points.zip(otherPoints)
        .dropWhile { case (l, r) => l == r }
        .headOption match {
          case Some((l, r)) => l > r
          case None => false
        }
    }

    override def compare(that: CamelCard): Int = {
      (this.getKind(), that.getKind()) match {
        case (l, r) if l == r => {
          if (this.isStronger(that)) 1 else -1
        }
        case (l, r) => l - r
      }
    }
  }

  object CamelCard {
    def apply(line: String): CamelCard = {
      val parts = line.split("\\s")
      CamelCard(parts(0).trim, parts(1).trim.toInt)
    }
  }

  val file = getResourceFile("day7/input")
  val lines = getLines(file)

  val cards = lines.map(CamelCard(_)).toList.sorted
  // debug
  cards.foreach(println)

  println(cards.length)
  println(cards.zipWithIndex.map { case (c, i) => c.bid * (i + 1) }.sum)
}
