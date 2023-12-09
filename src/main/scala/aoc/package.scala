import java.io.File
import java.nio.file.Paths
import scala.io.Source

package object aoc {

  val numberRegex = """(\d+)""".r

  def getLines(file: File) = Source.fromFile(file).getLines()

  def getResourceFile(filename: String) = Paths.get(s"src/main/resources/$filename").toFile

  def parseInt(line: String): Seq[Int] = numberRegex.findAllIn(line).map(_.toInt).toSeq

  def parseLong(line: String): Seq[Long] = numberRegex.findAllIn(line).map(_.toLong).toSeq
}
