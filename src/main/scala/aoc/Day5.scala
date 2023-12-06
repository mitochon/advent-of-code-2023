package aoc

object Day5 extends App {

  val numberRegex = """(\d+)""".r

  case class Range(dest: Long, src: Long, len: Int) {
    def inRange(from: Long): Boolean = src <= from && from < src + len
  }

  object Range {
    def apply(line: String): Range = {
      parseNumbers(line).toList match {
        case dest :: src :: len :: Nil => Range(dest, src, len.toInt)
        case _ => throw new IllegalArgumentException(s"Invalid line: $line")
      }
    }
  }

  case class ConversionMap(ranges: Seq[Range]) {
    def map(from: Long): Long = {
      ranges.find(_.inRange(from)) match {
        case Some(r) => r.dest - r.src + from
        case None => from
      }
    }
  }

  case class Seed(start: Long, len: Long) {
    def getIterator: Iterator[Long] = (start until start + len).iterator
  }

  object Seed {
    def apply(xs: Seq[Long]): Seed = {
      xs.toList match {
        case start :: len :: Nil => Seed(start, len)
        case _ => throw new IllegalArgumentException(s"Invalid seed: $xs")
      }
    }
  }

  def parseNumbers(line: String): Seq[Long] = {
    numberRegex.findAllIn(line).map(_.toLong).toSeq
  }

  def parseSection(lines: Iterator[String], startHeader: String): ConversionMap = {
    val section = lines.dropWhile(!_.startsWith(startHeader)).drop(1).takeWhile(_.nonEmpty).toList
    ConversionMap(section.map(Range(_)))
  }

  val file = getResourceFile("day5/input")
  val seed = parseNumbers(getLines(file).next())
  val seedGroups = seed.sliding(2, 2).map(Seed(_)).toList

  val s2s = parseSection(getLines(file), "seed-to-soil map:")
  val s2f = parseSection(getLines(file), "soil-to-fertilizer map:")
  val f2w = parseSection(getLines(file), "fertilizer-to-water map:")
  val w2l = parseSection(getLines(file), "water-to-light map:")
  val l2t = parseSection(getLines(file), "light-to-temperature map:")
  val t2h = parseSection(getLines(file), "temperature-to-humidity map:")
  val h2l = parseSection(getLines(file), "humidity-to-location map:")

  def cycle(from: Long): Long = {
    val a = s2s.map(from)
    val b = s2f.map(a)
    val c = f2w.map(b)
    val d = w2l.map(c)
    val e = l2t.map(d)
    val f = t2h.map(e)
    val g = h2l.map(f)
    // debug
    // println(s"from: $from, a: $a, b: $b, c: $c, d: $d, e: $e, f: $f, g: $g")
    g
  }

  // part1
  println(seed.map(cycle).min)
  // part2 - run brute-force
  println(seedGroups.map(_.getIterator.map(cycle).min).min)
}
