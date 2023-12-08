package aoc

object Day8 extends App {

  val nodeRegex = """(\w+) = \((\w+), (\w+)\)""".r

  case class Node(key: String, left: String, right: String) {
    def nextKey(direction: Char): String = {
      direction match {
        case 'L' => left
        case 'R' => right
      }
    }
  }

  object Node {
    def apply(line: String): Node = {
      line match {
        case nodeRegex(k, l, r) => Node(k, l, r)
        case _ => throw new Exception(s"Invalid node: $line")
      }
    }
  }

  val file = getResourceFile("day8/input")

  val seedDirections = getLines(file).next()
  val directions = LazyList.continually(seedDirections).flatten.iterator
  val nodes = getLines(file).filter(_.contains("=")).map(Node(_)).toList
  val nodeMap = nodes.map(n => (n.key -> n)).toMap

  def countSteps(nodeMap: Map[String, Node], directions: Iterator[Char]): Int = {
    var count = 0
    var node = nodeMap("AAA")
    while (node.key != "ZZZ") {
      val direction = directions.next()
      println(s" $node -> $direction -> $count")
      node = nodeMap(node.nextKey(direction))
      count = count + 1
    }
    count
  }

  println(nodeMap)
  println(countSteps(nodeMap, directions))
}
