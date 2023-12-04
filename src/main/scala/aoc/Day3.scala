package aoc

import scala.annotation.tailrec

object Day3 extends App {

  val numberRegex = """(\d+)""".r

  case class Coord(row: Int, col: Int)
  case class Symbol(s: Char, c: Coord) {
    def adjacentCoords(): Seq[Coord] = {
      (-1 to 1).flatMap { x =>
        (-1 to 1).map { y =>
          Coord(c.row + x, c.col + y)
        }
      }
    }
  }
  case class Part(n: Int, c: Coord) {
    def coords(): Seq[Coord] = (0 until n.toString.size).map { i => Coord(c.row, c.col + i) }
  }

  def parseSymbols(line: String, row: Int): Seq[Symbol] = {
    line.zipWithIndex
      .filterNot(_._1.isDigit)
      .filterNot(_._1 == '.')
      .map { case (s, col) => Symbol(s, Coord(row, col)) }
  }

  def parseParts2(line: String, row: Int): Seq[Part] = {
    val numbers = numberRegex.findAllIn(line).map(_.toInt).toSeq
    val indices = numbers.map(n => line.indexOf(n.toString))
    numbers.zip(indices).map { case (n, col) => Part(n, Coord(row, col)) }
  }

  def parseParts(line: String, row: Int): Seq[Part] = {
    def makePart(digits: String, index: Int) = Part(digits.toInt, Coord(row, index + 1 - digits.size))
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

  val file = getResourceFile("day3/input")
  val symbols = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseSymbols(line, row)
  }
  val parts = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseParts(line, row)
  }

  val validCoords = symbols.flatMap(_.adjacentCoords()).toSet
  val validParts = parts.filter(p => p.coords().exists(c => validCoords.contains(c))).toList
  // debug
  validParts.groupBy(_.c.row).toList.sortBy(_._1).foreach { case (row, parts) => println(s"$row: ${parts.map(_.n)}") }

  println(validParts.map(_.n).sum)
}
