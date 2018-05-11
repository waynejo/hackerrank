import scala.annotation.tailrec
import scala.collection.immutable.HashMap

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
  def generatePairMap(pairs: Array[Array[Int]], idx: Int, acc: Array[Int]): Array[Int] = {
    if (idx == pairs.length) {
      acc
    } else {
      val v0 = pairs(idx)(0)
      val v1 = pairs(idx)(1)
      val (depth0, id0) = depthAndId(acc, v0)
      val (depth1, id1) = depthAndId(acc, v1)

      if (id0 == id1) {
        generatePairMap(pairs, idx + 1, acc)
      } else if (depth0 < depth1) {
        acc(id0) = id1
        generatePairMap(pairs, idx + 1, acc)
      } else {
        acc(id1) = id0
        generatePairMap(pairs, idx + 1, acc)
      }
    }
  }

  def roadsAndLibraries(n: Int, c_lib: Int, c_road: Int, cities: Array[Array[Int]]): Long = {
    val links = generatePairMap(cities, 0, (0 until n).toArray)
    val countOfLinkedCities: Array[Long] = (HashMap[Int, Long]() /: (0 until n))((acc, x) => {
      val id = groupId(links, x)
      acc.updated(id, acc.getOrElse(id, 0L) + 1L)
    }).values.toArray
    if (c_lib > c_road) {
      countOfLinkedCities.map(x => c_lib + c_road * (x - 1)).sum
    } else {
      countOfLinkedCities.map(_ * c_lib).sum
    }
  }

  def main(args: Array[String]) {

    val stdin = scala.io.StdIn
    val q = stdin.readLine.toInt
    for (qItr <- 1 to q) {
      val nmC_libC_road = stdin.readLine.split(" ")
      val n = nmC_libC_road(0).toInt
      val m = nmC_libC_road(1).toInt
      val c_lib = nmC_libC_road(2).toInt
      val c_road = nmC_libC_road(3).toInt
      val cities = Array.ofDim[Int](m, 2)

      for (i <- 0 until m) {
        cities(i) = stdin.readLine.split(" ").map(_.toInt - 1)
      }

      val result = roadsAndLibraries(n, c_lib, c_road, cities)
      println(result)
    }
  }
}
