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

    def generatePairMap(pairs: List[Array[Int]], acc: Array[Int], sums: HashMap[Int, Int]): Long = {
        if (pairs.isEmpty) {
            ((0, sums, acc.length) /: sums.keys){case ((answer, remains, others), key) =>
                val value = remains(key)
                val nextHash = remains - key
                (answer + value * (others - value), nextHash, others - value)
            }._1
        } else {
            val v0 = pairs.head(0)
            val v1 = pairs.head(1)
            val (depth0, id0) = depthAndId(acc, v0)
            val (depth1, id1) = depthAndId(acc, v1)

            val sum = sums(id0) + sums(id1)
            if (id0 == id1) {
                generatePairMap(pairs.tail, acc, sums)
            } else if (depth0 < depth1) {
                acc(id0) = id1
                generatePairMap(pairs.tail, acc, sums.updated(id1, sum) - id0)
            } else {
                acc(id1) = id0
                generatePairMap(pairs.tail, acc, sums.updated(id0, sum) - id1)
            }
        }
    }

    def journeyToMoon(n: Int, astronaut: Array[Array[Int]]): Long = {
        val range = 0 until n
        val initialSums = (HashMap[Int, Int]() /: range)((acc, x) => acc.updated(x, 1))
        generatePairMap(astronaut.toList, range.toArray, initialSums)
    }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var p = sc.nextInt();
        var astronaut = Array.ofDim[Int](p,2);
        for(astronaut_i <- 0 to p-1) {
           for(astronaut_j <- 0 to 2-1){
              astronaut(astronaut_i)(astronaut_j) = sc.nextInt();
           }
        }
        val result = journeyToMoon(n, astronaut);
        println(result)
    }
}
