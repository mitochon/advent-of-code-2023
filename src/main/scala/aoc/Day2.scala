package aoc

object Day2 extends App {

  private val redRegex = """(\d+) red""".r
  private val blueRegex = """(\d+) blue""".r
  private val greenRegex = """(\d+) green""".r
  private val gameRegex = """Game (\d+): (.+)""".r

  /* Red Blue Green */
  case class RBG(r: Int, b: Int, g: Int) {
    def power(): Int = r * b * g
    def merge(other: RBG): RBG = RBG(math.max(this.r, other.r), math.max(this.b, other.b), math.max(this.g, other.g))
  }
  object RBG {
    def fromGameLine(line: String): RBG = line.split("[;]").map(RBG(_)).foldLeft(RBG(0, 0, 0))(_.merge(_))
    def apply(line: String): RBG = {
      line.split("[,]")
        .map(_.trim)
        .foldLeft(RBG(0, 0, 0)) {
          case (rbg, entry) =>
            entry match {
              case redRegex(n) => rbg.copy(r = n.toInt)
              case blueRegex(n) => rbg.copy(b = n.toInt)
              case greenRegex(n) => rbg.copy(g = n.toInt)
              case _ => throw new IllegalArgumentException(s"Invalid line: $line")
            }
        }
    }
  }

  case class Game(id: Int, rbg: RBG) {
    def checkPossible(constraint: RBG): Boolean = rbg.r <= constraint.r && rbg.b <= constraint.b && rbg.g <= constraint.g
  }
  object Game {
    def apply(line: String): Game = {
      line match {
        case gameRegex(id, rest) => Game(id.toInt, RBG.fromGameLine(rest))
        case _ => throw new IllegalArgumentException(s"Invalid line: $line")
      }
    }
  }

  val file = getResourceFile("day2/input")
  val constraint = RBG(12, 14, 13)
  val games = getLines(file).map(Game(_)).toList
  val powers = games.map(_.rbg.power())
  val possibleGames = games.filter(_.checkPossible(constraint))

  println(possibleGames.map(_.id).sum)
  println(powers.sum)
}