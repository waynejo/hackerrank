import scala.annotation.tailrec
import scala.io.StdIn

object Solution {

  @tailrec
  def depthAndId(pairMap: Array[Int], v: Int, acc: Int = 0): (Int, Int) = {
    if (v == pairMap(v)) {
      (acc, v)
    } else {
      depthAndId(pairMap, pairMap(v), acc + 1)
    }
  }

  @tailrec
  def groupId(pairMap: Array[Int], v: Int): Int = {
    if (v == pairMap(v)) {
      v
    } else {
      groupId(pairMap, pairMap(v))
    }
  }

  @tailrec
  def generatePairMap(links: Array[Array[Int]], idx: Int, acc: Array[Int], sums: Long = 0): Long = {
    if (idx >= links.length) {
      sums
    } else {
      val v0 = links(idx)(0)
      val v1 = links(idx)(1)
      val (depth0, id0) = depthAndId(acc, v0)
      val (depth1, id1) = depthAndId(acc, v1)

      if (id0 == id1) {
        generatePairMap(links, idx + 1, acc, sums)
      } else if (depth0 < depth1) {
        acc(id0) = id1
        generatePairMap(links, idx + 1, acc, sums + links(idx)(2))
      } else {
        acc(id1) = id0
        generatePairMap(links, idx + 1, acc, sums + links(idx)(2))
      }
    }
  }


  def mst(n: Int, links: Array[Array[Int]]): Long = {
    val range = 0 to n
    generatePairMap(links, 0, range.toArray)
  }

  def main(args: Array[String]): Unit = {
    val Array(n, m) = StdIn.readLine().split(" ").map(_.toInt)
    val links = for (_ <- 0 until m) yield {
      StdIn.readLine().split(" ").map(_.toInt)
    }

    println(mst(n, links.toArray.sortBy(x => (x(2), x.sum))))
  }
}
