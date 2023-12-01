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

  def getDigits2(line: String, first: Boolean): Int = {
    val x = line.filter(_.isDigit) match {
      case "" => 0
      case xs if first => xs.head.toString.toInt
      case xs => xs.last.toString.toInt
    }
    x
  }

  def foo(line: String): Int = {
    val firstDigitInput = numberMap
      .map { case (k, v) => line.indexOf(k) -> v }
      .view.filterKeys(_ >= 0)
      .toSeq
      .minByOption(_._1)
      .map(_._2)
      .map(literals(_))

    val lastDigitInput = numberMap
      .map { case (k, v) => line.lastIndexOf(k) -> v }
      .view.filterKeys(_ >= 0)
      .toSeq
      .maxByOption(_._1)
      .map(_._2)
      .map(literals(_))

    val f1 = firstDigitInput.fold(line)(s => line.replace(s, s"${numberMap(s) + 1}"))
    val f2 = lastDigitInput.fold(line)(s => line.replace(s, s"${numberMap(s) + 1}"))
    val d1 = getDigits2(f1, true)
    val d2 = getDigits2(f2, false)
    d1 * 10 + d2
  }
  //
  //  def replaceLiteral2(line: String): String = {
  //    val firstLiteral = getPairs(line, line.indexOf, isMin = true).map(literals)
  //    val lastLiteral = getPairs(line, line.lastIndexOf, isMin = false).map(literals)
  //    firstLiteral.fold(line)(literal => line.replace(literal, s"${numberMap(literal) + 1}"))
  //    lastLiteral.fold(line)(literal => line.replace(literal, s"${numberMap(literal) + 1}"))
  //
  //  }
  //
  //  def replaceFirstLiteral(line: String): String = {
  //    val firstLiteral = getPairs(line, line.indexOf, isMin = true).map(literals)
  //    firstLiteral.fold(line)(literal => line.replaceFirst(literal, s"${numberMap(literal) + 1}"))
  //  }

  // attempt 1
  def replaceLiteral(line: String): String = {
    val pairs = numberMap
      .map { case (k, v) => line.indexOf(k) -> v }
      .view.filterKeys(_ >= 0)
      .toSeq

    if (pairs.isEmpty) line
    else {
      val firstLiteral = literals(pairs.minBy(_._1)._2)
      val lastLiteral = literals(pairs.maxBy(_._1)._2)
      line.replace(firstLiteral, s"${numberMap(firstLiteral) + 1}")
        .replace(lastLiteral, s"${numberMap(lastLiteral) + 1}")
    }
  }

  // attempt 2
  def replaceFirstLiteral(line: String): String = {
    val pairs = numberMap
      .map { case (k, v) => line.indexOf(k) -> v }
      .view.filterKeys(_ >= 0)
      .toSeq

    if (pairs.isEmpty) line
    else {
      val firstLiteral = literals(pairs.minBy(_._1)._2)
      line.replaceFirst(firstLiteral, s"${numberMap(firstLiteral) + 1}")
    }
  }

  def replaceLastLiteral(line: String): String = {
    val pairs = numberMap
      .map { case (k, v) => line.lastIndexOf(k) -> v }
      .view.filterKeys(_ >= 0)
      .toSeq

    if (pairs.isEmpty) line
    else {
      val lastLiteral = literals(pairs.maxBy(_._1)._2)
      line.replace(lastLiteral, s"${numberMap(lastLiteral) + 1}")
    }
  }

  def getLines(file: File) = Source.fromFile(file).getLines()

  val file = Paths.get("src/main/resources/day1/input").toFile
//  println("Part 1: " + getLines(file).map(getDigits).foldLeft(0)(_ + _))
//    println("Part 2a: " + Source.fromFile(file).getLines().map(replaceLiteral).map(getDigits).foldLeft(0)(_ + _))
  //println("Part 2b: " + getLines(file).map(replaceLastLiteral).map(replaceFirstLiteral).map(getDigits).foldLeft(0)(_ + _))
  println("Part 2c: " + Source.fromFile(file).getLines().map(foo).foldLeft(0)(_ + _))
  //  val ww = getLines(file).toList
  //  val xx = ww.map(replaceLastLiteral).toList
  //  val yy = xx.map(replaceFirstLiteral).toList
  //  val zz = yy.map(getDigits).toList
  //  val aa = zz.foldLeft(0)(_ + _)

  /*
  Part 1: 54667
  Part 2a: 54222 xx too high
  54203
  Part 2b: 54193 xx too low
   */
  //  getLines(file).foreach(line => { println(line); println(replaceLiteral(line)) })

  // That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. [Return to Day 1]

  // That's not the right answer; your answer is too low. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. [Return to Day 1]
}
