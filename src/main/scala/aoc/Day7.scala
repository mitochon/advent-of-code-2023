package aoc

object Day7 extends App {

  val labels = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse
  val labelPoints = labels.zipWithIndex.toMap

  val labelsWithJoker = List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse
  val labelPointsWithJoker = labelsWithJoker.zipWithIndex.toMap

  case class CamelCard(hand: String, bid: Int) {

    def getKind(): Int = getKind(CamelCard.getCounts(hand))

    def getKind(counts: List[Int]): Int = {
      counts match {
        case 5 :: xs => 6
        case 4 :: xs => 5
        case 3 :: 2 :: xs => 4
        case 3 :: xs => 3
        case 2 :: 2 :: xs => 2
        case 2 :: xs => 1
        case _ => 0
      }
    }

    def getKindWithJoker(): Int = {
      val numJokers = hand.count(_ == 'J')
      CamelCard.getCounts(hand.filterNot(_ == 'J')) match {
        case x :: xs => getKind(x + numJokers :: xs)
        case Nil => getKind(numJokers :: Nil)
      }
    }

    def isStronger(other: CamelCard, lookup: Map[Char, Int]): Boolean = {
      hand.zip(other.hand)
        .dropWhile { case (l, r) => l == r }
        .headOption match {
          case Some((l, r)) => lookup(l) > lookup(r)
          case None => false
        }
    }
  }

  object CamelCard {
    def apply(line: String): CamelCard = {
      val parts = line.split("\\s")
      CamelCard(parts(0).trim, parts(1).trim.toInt)

    }
    def getCounts(hand: String): List[Int] = {
      hand.groupBy(identity).view
        .mapValues(_.size)
        .values.toList
        .sorted.reverse
    }

    def sortWithJoker(card1: CamelCard, card2: CamelCard): Boolean = {
      (card1.getKindWithJoker(), card2.getKindWithJoker()) match {
        case (l, r) if l == r => card2.isStronger(card1, labelPointsWithJoker)
        case (l, r) => l < r
      }
    }

    def sortDefault(card1: CamelCard, card2: CamelCard): Boolean = {
      (card1.getKind(), card2.getKind()) match {
        case (l, r) if l == r => card2.isStronger(card1, labelPoints)
        case (l, r) => l < r
      }
    }
  }

  val file = getResourceFile("day7/input")

  val cards = getLines(file).map(CamelCard(_)).toList.sortWith(CamelCard.sortDefault)
  val cardsWithJoker = getLines(file).map(CamelCard(_)).toList.sortWith(CamelCard.sortWithJoker)

  println(cards.zipWithIndex.map { case (c, i) => c.bid * (i + 1) }.sum)
  println(cardsWithJoker.zipWithIndex.map { case (c, i) => c.bid * (i + 1) }.sum)
}
