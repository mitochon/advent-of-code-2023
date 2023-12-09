package aoc

import scala.annotation.tailrec

object Day3 extends App {

  case class Coord(row: Int, col: Int)
  case class Symbol(s: Char, c: Coord) {
    def adjacentCoords(): Seq[Coord] = {
      (-1 to 1).flatMap { x =>
        (-1 to 1).map { y =>
          Coord(c.row + x, c.col + y)
        }
      }
    }
    def withNParts(n: Int, coordMap: Map[Coord, Part]): Set[Part] = {
      val matchingParts = adjacentCoords().flatMap(coordMap.get).toSet
      if (matchingParts.size == n) matchingParts else Set()
    }
  }
  case class Part(n: Int, c: Coord) {
    def coords(): Seq[Coord] = (0 until n.toString.length).map { i => Coord(c.row, c.col + i) }
  }

  def parseSymbols(line: String, row: Int): Seq[Symbol] = {
    line.zipWithIndex
      .filterNot(_._1.isDigit)
      .filterNot(_._1 == '.')
      .map { case (s, col) => Symbol(s, Coord(row, col)) }
  }

  def parseParts(line: String, row: Int): Seq[Part] = {
    def makePart(digits: String, index: Int) = Part(digits.toInt, Coord(row, index + 1 - digits.length))
    @tailrec
    def build(acc: List[Part], digits: String, lastIndex: Int, remainder: List[(Char, Int)]): List[Part] = {
      remainder match {
        case Nil if digits.isEmpty => acc
        case Nil => makePart(digits, lastIndex) :: acc
        case (c, colIndex) :: xs if digits.isEmpty || lastIndex + 1 == colIndex => build(acc, digits + c, colIndex, xs)
        case (c, colIndex) :: xs => build(makePart(digits, lastIndex) :: acc, "" + c, colIndex, xs)
      }
    }
    // initialize recursive call
    build(Nil, "", -1, line.zipWithIndex.filter(_._1.isDigit).toList).reverse
  }

  def calcGearRatio(parts: Set[Part]): Int = {
    parts.foldLeft(1)(_ * _.n)
  }

  val file = getResourceFile("day3/input")

  val symbols = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseSymbols(line, row)
  }.toList
  val parts = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseParts(line, row)
  }.toList

  // part1
  val validCoords = symbols.flatMap(_.adjacentCoords()).toSet
  val validParts = parts.filter(p => p.coords().exists(c => validCoords.contains(c)))
  // debug
  validParts.groupBy(_.c.row).toList.sortBy(_._1).foreach { case (row, parts) => println(s"$row: ${parts.map(_.n)}") }

  // part2
  val coordMap = parts.flatMap(p => p.coords().map(c => (c, p))).toMap
  val gearRatios = symbols.filter(_.s == '*')
    .map(_.withNParts(2, coordMap))
    .filter(_.nonEmpty)
    .map(calcGearRatio)

  println(validParts.map(_.n).sum)
  println(gearRatios.sum)
}
