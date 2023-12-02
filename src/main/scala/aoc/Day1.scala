package aoc

import java.io.File
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

  // part2
  val literals = Array("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val numberMap = literals.zipWithIndex.toMap

  def getDigitsWithLiterals(line: String): Int = {

    def toDigit(line: String, fn: String => Int): Int = {
      val digits = line.filter(_.isDigit)
      if (digits.isEmpty) 0 else fn(digits)
    }

    def getMatches(line: String, fn: String => Int) = {
      numberMap.map { case (k, v) => fn(k) -> v }.view.filterKeys(_ >= 0)
    }

    def replaceWithNum(line: String, target: Option[String]): String = {
      target.fold(line)(s => line.replace(s, s"${numberMap(s) + 1}"))
    }
    // get the first match from left to right and the last match from right to left
    val fromL2R = getMatches(line, line.indexOf).minByOption(_._1).map(_._2).map(literals(_))
    val fromR2L = getMatches(line, line.lastIndexOf).maxByOption(_._1).map(_._2).map(literals(_))
    val d1 = toDigit(replaceWithNum(line, fromL2R), _.head.toString.toInt)
    val d2 = toDigit(replaceWithNum(line, fromR2L), _.last.toString.toInt)
    d1 * 10 + d2
  }

  def getLines(file: File) = Source.fromFile(file).getLines()

  val file = Paths.get("src/main/resources/day1/input").toFile
  println("Part 1: " + getLines(file).map(getDigits).foldLeft(0)(_ + _))
  println("Part 2: " + getLines(file).map(getDigitsWithLiterals).foldLeft(0)(_ + _))
}
