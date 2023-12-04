import java.io.File
import java.nio.file.Paths
import scala.io.Source

package object aoc {

  def getLines(file: File) = Source.fromFile(file).getLines()

  def getResourceFile(filename: String) = Paths.get(s"src/main/resources/$filename").toFile
}
