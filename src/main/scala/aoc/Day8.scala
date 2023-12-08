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

  val nodes = getLines(file).filter(_.contains("=")).map(Node(_)).toList
  val nodeMap = nodes.map(n => (n.key -> n)).toMap
  val seedDirections = getLines(file).next()

  def getDirections(): Iterator[Char] = LazyList.continually(seedDirections).flatten.iterator

  // part1
  def countSteps(nodeMap: Map[String, Node], directions: Iterator[Char]): Int = {
    var count = 0
    var node = nodeMap("AAA")
    while (node.key != "ZZZ") {
      val direction = directions.next()
      // debug
      println(s" $node -> $direction -> $count")
      node = nodeMap(node.nextKey(direction))
      count = count + 1
    }
    count
  }

  println(countSteps(nodeMap, getDirections()))

  // part2
  val startNodes = nodes.filter(_.key.endsWith("A"))
  val endNodes = nodes.filter(_.key.endsWith("Z"))
  def countSuffix(nodeMap: Map[String, Node], start: Node, directions: Iterator[Char]): Int = {
    var count = 0
    var node = nodeMap(start.key)
    while (!node.key.endsWith("Z")) {
      val direction = directions.next()
      node = nodeMap(node.nextKey(direction))
      count = count + 1
    }
    // debug
    println(s"End: $node - $count")
    count
  }

  val cycles = startNodes.map(countSuffix(nodeMap, _, getDirections()))
  println(cycles) // get the LCM of the cycles
}
