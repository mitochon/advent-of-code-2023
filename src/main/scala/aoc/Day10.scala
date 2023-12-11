package aoc

import scala.annotation.tailrec

object Day10 extends App {

  object Direction extends Enumeration {
    type Direction = Value
    val N, S, E, W = Value
  }

  case class Coord(x: Int, y: Int) {
    def getBearing(other: Coord): Option[Direction.Value] = {
      (x - other.x, y - other.y) match {
        case (1, 0) => Some(Direction.N)
        case (-1, 0) => Some(Direction.S)
        case (0, 1) => Some(Direction.W)
        case (0, -1) => Some(Direction.E)
        case _ => None
      }
    }
  }

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
      } yield traverse(startTile, seed, Seq(startTile, seed))

      path.getOrElse(Seq())
    }

    def getPipScores(path: Seq[Tile]): Map[Tile, Int] = {
      // From https://en.wikipedia.org/wiki/Point_in_polygon
      val pathCoordMap = path.map(t => (t.coord -> t)).toMap
      val byRow = path.groupBy(_.coord.x).view.mapValues(_.sortBy(_.coord.y)).toMap
      val startCharReplacement = {
        // get bearing of all tiles adjacent to the start tile, assumes certain ordering
        val bearings = for {
          s <- path.headOption
          h <- path.drop(1).headOption
          e <- path.lastOption
          x <- s.coord.getBearing(h.coord)
          y <- s.coord.getBearing(e.coord)
        } yield List(x, y)

        bearings.getOrElse(List()).sorted match {
          case Direction.N :: Direction.S :: Nil => '|'
          case Direction.E :: Direction.W :: Nil => '-'
          case Direction.N :: Direction.E :: Nil => 'L'
          case Direction.N :: Direction.W :: Nil => 'J'
          case Direction.S :: Direction.E :: Nil => 'F'
          case Direction.S :: Direction.W :: Nil => '7'
          case _ => throw new Exception("Invalid start tile")
        }
      }

      def getScore(c: Coord): Int = byRow.get(c.x).fold(0)(is => countIntersects(c, is))

      def countIntersects(c: Coord, intersects: Seq[Tile]): Int = {
        intersects.filter(_.coord.y < c.y)
          .filter(_.kind != '-')
          .map(_.kind).mkString
          .replaceAll("S", startCharReplacement.toString) // replace S
          .replaceAll("FJ", "|") // these next two acts as one
          .replaceAll("L7", "|")
          .replaceAll("F7", "") // these next two nullify each other
          .replaceAll("LJ", "")
          .length
      }

      tiles.map(t => pathCoordMap.get(t.coord) match {
        case Some(t) => (t -> 0)
        case None => (t -> getScore(t.coord))
      }).toMap
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
  val scoredTiles = field.getPipScores(path).toSeq.sortBy {
    case (t, _) => (t.coord.x, t.coord.y)
  }

  // debug
  scoredTiles.foreach(println)
  println(scoredTiles.count(_._2 % 2 == 1))
}
