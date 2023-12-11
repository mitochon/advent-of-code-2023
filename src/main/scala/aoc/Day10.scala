package aoc

import scala.annotation.tailrec

object Day10 extends App {

  object Direction extends Enumeration {
    type Direction = Value
    val N, S, E, W = Value
  }

  case class Coord(x: Int, y: Int)

  case class Tile(coord: Coord, kind: Char) {
    def next(from: Direction.Value): Option[Direction.Value] = {
      (kind, from) match {
        case ('|', Direction.S) => Some(Direction.N)
        case ('|', Direction.N) => Some(Direction.S)
        case ('-', Direction.E) => Some(Direction.W)
        case ('-', Direction.W) => Some(Direction.E)
        case ('L', Direction.N) => Some(Direction.E)
        case ('L', Direction.E) => Some(Direction.N)
        case ('J', Direction.N) => Some(Direction.W)
        case ('J', Direction.W) => Some(Direction.N)
        case ('F', Direction.S) => Some(Direction.E)
        case ('F', Direction.E) => Some(Direction.S)
        case ('7', Direction.S) => Some(Direction.W)
        case ('7', Direction.W) => Some(Direction.S)
        case _ => None
      }
    }

    def adjacentCoords(): Seq[(Coord, Direction.Value)] = {
      // direction is reversed (from POV of the target tile)
      Seq(
        (Coord(coord.x - 1, coord.y), Direction.S),
        (Coord(coord.x + 1, coord.y), Direction.N),
        (Coord(coord.x, coord.y - 1), Direction.E),
        (Coord(coord.x, coord.y + 1), Direction.W)
      ).filter {
          case (_, direction) => Tile.validDirection(kind).contains(direction)
        }
    }
  }

  object Tile {
    def validDirection(kind: Char): Set[Direction.Value] = {
      // direction is reversed (from POV of the target tile)
      kind match {
        case 'F' => Set(Direction.N, Direction.W)
        case '7' => Set(Direction.N, Direction.E)
        case 'J' => Set(Direction.S, Direction.E)
        case 'L' => Set(Direction.S, Direction.W)
        case '|' => Set(Direction.N, Direction.S)
        case '-' => Set(Direction.E, Direction.W)
        case 'S' => Set(Direction.E, Direction.W, Direction.N, Direction.S)
        case _ => Set()
      }
    }
  }

  case class Field(tiles: Seq[Tile]) {
    val tileMap = tiles.map(t => (t.coord -> t)).toMap

    def findStart(): Option[Tile] = tiles.find(_.kind == 'S')

    def adjacentTiles(tile: Tile): List[Tile] = {
      tile.adjacentCoords.flatMap {
        case (coord, direction) => {
          val tile = tileMap.get(coord)
          tile.filter(_.next(direction).nonEmpty)
        }
      }.toList
    }

    def findPath(): Seq[Tile] = {
      @tailrec
      def traverse(startTile: Tile, currentTile: Tile, path: Seq[Tile]): Seq[Tile] = {
        if (currentTile == startTile) path
        else {
          // debug
          println(currentTile)
          adjacentTiles(currentTile).filterNot(path.contains) match {
            case next :: _ => traverse(startTile, next, path :+ next)
            case Nil => println("done"); path
          }
        }
      }
      val path = for {
        startTile <- findStart()
        seed <- adjacentTiles(startTile).headOption
      } yield traverse(startTile, seed, Seq(seed, startTile))

      path.getOrElse(Seq())
    }

    def getPipScores(path: Seq[Tile]): Map[Tile, Int] = {
      // From https://en.wikipedia.org/wiki/Point_in_polygon
      Map()
    }
  }

  object Field {
    def apply(xs: Iterator[String]): Field = {
      Field(xs.zipWithIndex
        .flatMap {
          case (line, x) => line.zipWithIndex.map {
            case (c, y) => Tile(Coord(x + 1, y + 1), c)
          }
        }.toList)
    }
  }

  val file = getResourceFile("day10/input")
  val field = Field(getLines(file))
  val path = field.findPath()

  // part1
  if (path.length % 2 == 0) println(path.length / 2)
  else println(path.length / 2 + 1)

  // part2
  field.getPipScores(path).values.count(_ % 2 == 1)
}
