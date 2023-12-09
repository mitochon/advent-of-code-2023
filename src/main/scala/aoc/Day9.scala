package aoc

import scala.collection.mutable.ArrayBuffer

object Day9 extends App {

  case class Report(history: Seq[Int]) {

    def derivations(): Seq[Seq[Int]] = {
      val derivations = ArrayBuffer(history)
      var last = history
      while (!Report.checkZeros(last)) {
        val diff = Report.getDiff(last)
        derivations += diff
        last = diff
      }
      derivations.toList
    }

    def extrapolate(): Int = {
      derivations().map(_.last).sum
    }

    def extrapolateBackward(): Int = {
      val firsts = derivations().flatMap(_.headOption).reverse
      firsts.foldLeft(0) { case (acc, i) => i - acc }
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

  // part2
  val backwardExtrapolations = reports.map(_.extrapolateBackward)
  println(backwardExtrapolations)
  println(backwardExtrapolations.sum)
}
