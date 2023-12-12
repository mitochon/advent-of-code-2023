package aoc

object Day11 extends App {

  case class Coord(x: Int, y: Int) {

    def expand(emptyRows: Set[Int], emptyCols: Set[Int]) = {
      Coord(x + emptyRows.count(_ < x), y + emptyCols.count(_ < y))
    }

    def distance(other: Coord): Int = {
      Math.abs(x - other.x) + Math.abs(y - other.y)
    }
  }

  case class SpaceMap(galaxies: Seq[Coord], rows: Int, cols: Int) {
    def expand(): SpaceMap = {
      val emptyRows = (1 to rows).toSet -- galaxies.map(_.x).toSet
      val emptyCols = (1 to cols).toSet -- galaxies.map(_.y).toSet
      SpaceMap(
        galaxies.map(_.expand(emptyRows, emptyCols)),
        rows + emptyRows.size,
        cols + emptyCols.size
      )
    }

    def getPairings(): Seq[(Coord, Coord)] = {
      galaxies.combinations(2).flatMap {
        case Seq(a, b) => Some((a, b))
        case _ => None
      }.toSeq
    }

    def shortestTotalDistance(): Int = {
      getPairings().map {
        case (a, b) => a.distance(b)
      }.sum
    }
  }

  object SpaceMap {
    def apply(xs: Iterator[String]): SpaceMap = {
      val coordMap = xs.zipWithIndex
        .flatMap {
          case (line, x) => line.zipWithIndex.map {
            case (c, y) => (Coord(x + 1, y + 1) -> c)
          }
        }.toMap

      SpaceMap(
        coordMap.filter(_._2 == '#').keys.toSeq,
        coordMap.keys.map(_.x).max,
        coordMap.keys.map(_.y).max
      )
    }
  }

  val file = getResourceFile("day11/input")
  val spaceMap = SpaceMap(getLines(file))

  // part1
  println(spaceMap.expand().shortestTotalDistance())
}
