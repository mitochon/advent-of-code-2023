package aoc

import java.nio.file.Paths
import scala.io.Source

object Day1 extends App {

  // part1
  def getDigits(line: String): Int = {
    line.filter(_.isDigit) match {
      case "" => 0
      case xs => s"${xs.head}${xs.last}".toInt
    }
  }

  val file = Paths.get("src/main/resources/day1/input").toFile
  val lines = Source.fromFile(file).getLines()
  println(lines.map(getDigits).foldLeft(0)(_ + _))
}
