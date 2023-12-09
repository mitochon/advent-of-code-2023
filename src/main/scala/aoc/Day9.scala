package aoc

import scala.collection.mutable.ArrayBuffer

object Day9 extends App {

  case class Report(history: Seq[Int]) {

    def extrapolate(): Int = {
      val derivations = ArrayBuffer(history)
      var last = history
      while (!Report.checkZeros(last)) {
        val diff = Report.getDiff(last)
        derivations += diff
        last = diff
      }
      derivations.map(_.last).sum
    }
  }

  object Report {
    def apply(line: String): Report = Report(line.split("\\s").map(_.toInt).toSeq)
    def checkZeros(xs: Seq[Int]): Boolean = xs.forall(_ == 0)
    def getDiff(xs: Seq[Int]): Seq[Int] = xs.zip(xs.tail).map { case (a, b) => b - a }
  }

  val file = getResourceFile("day9/input")
  val reports = getLines(file).map(Report(_)).toList
  val extrapolations = reports.map(_.extrapolate)

  // part1
  println(extrapolations)
  println(extrapolations.sum)
}
