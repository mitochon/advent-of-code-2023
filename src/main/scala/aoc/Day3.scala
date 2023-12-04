package aoc

object Day3 extends App {

  val numberRegex = """(\d+)""".r

  case class Coord(row: Int, col: Int)

  case class Symbol(s: Char, c: Coord) {
    def adjacentCoords(): Seq[Coord] = {
      (-1 to 1).flatMap { x =>
        (-1 to 1).map { y =>
          Coord(c.row + x, c.col + y)
        }
      }//.filter(_.row >= 0).filter(_.col >= 0).filterNot(_ == c)
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

  def parseParts(line: String, row: Int): Seq[Part] = {
    val numbers = numberRegex.findAllIn(line).map(_.toInt).toSeq
    val indices = numbers.map(n => line.indexOf(n.toString))
    numbers.zip(indices).map { case (n, col) => Part(n, Coord(row, col)) }
  }

//  val file = getResourceFile("day3/inputPart1")
    val file = getResourceFile("day3/input")

  val symbols = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseSymbols(line, row)
  }

  val partsCoordinates = symbols.flatMap(_.adjacentCoords()).toSet

  val parts = getLines(file).zipWithIndex.flatMap {
    case (line, row) => parseParts(line, row)
  }

//  parts.foreach(p => {println(p); println(p.coords())})

  val filteredParts = parts.filter(p => p.coords().exists(c => partsCoordinates.contains(c)))
  println(filteredParts.map(_.n).sum)

  // 521242
  //That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. [Return to Day 3]
}
