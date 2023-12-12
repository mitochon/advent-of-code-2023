package aoc

object Day11 extends App {

  case class Coord(x: Long, y: Long) {

    def expand(emptyRows: Set[Int], emptyCols: Set[Int], factor: Int = 2): Coord = {
      Coord(
        x + emptyRows.count(_ < x) * (factor - 1),
        y + emptyCols.count(_ < y) * (factor - 1)
      )
    }

    def distance(other: Coord): Long = {
      Math.abs(x - other.x) + Math.abs(y - other.y)
    }
  }

  case class SpaceMap(galaxies: Seq[Coord], rows: Int, cols: Int) {

    def expand(factor: Int = 2): SpaceMap = {
      val emptyRows = (1 to rows).toSet -- galaxies.map(_.x).map(_.toInt).toSet
      val emptyCols = (1 to cols).toSet -- galaxies.map(_.y).map(_.toInt).toSet
      SpaceMap(
        galaxies.map(_.expand(emptyRows, emptyCols, factor)),
        rows + (emptyRows.size * (factor - 1)),
        cols + (emptyCols.size * (factor - 1))
      )
    }

    def getPairings(): Seq[(Coord, Coord)] = {
      galaxies.combinations(2).flatMap {
        case Seq(a, b) => Some((a, b))
        case _ => None
      }.toSeq
    }

    def shortestTotalDistance(): Long = {
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
            case (c, y) => Coord(x + 1, y + 1) -> c
          }
        }.toMap

      SpaceMap(
        coordMap.filter(_._2 == '#').keys.toSeq,
        coordMap.keys.map(_.x).max.toInt,
        coordMap.keys.map(_.y).max.toInt
      )
    }
  }

  val file = getResourceFile("day11/input")
  val spaceMap = SpaceMap(getLines(file))

  // part1
  println(spaceMap.expand().shortestTotalDistance())

  // part2
  println(spaceMap.expand(10).shortestTotalDistance())
  println(spaceMap.expand(100).shortestTotalDistance())
  println(spaceMap.expand(1000000).shortestTotalDistance())
}
