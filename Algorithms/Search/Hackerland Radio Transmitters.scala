import scala.annotation.tailrec

object Solution {

    @tailrec
    def lastFilteredIndex(xs: Array[Int], value: Int, idx: Int): Int = {
        if (idx >= xs.length || value < xs(idx)) {
            idx - 1
        } else {
            lastFilteredIndex(xs, value, idx + 1)
        }
    }

    def hackerlandRadioTransmitters(x: Array[Int], k: Int): Int =  {
        @tailrec
        def _solve(xs: Array[Int], idx: Int, acc: Int): Int = {
            if (idx >= xs.length) {
                acc
            } else {
                val transmitterIdx = lastFilteredIndex(xs, xs(idx) + k, idx + 1)
                val transmitterEndIdx = lastFilteredIndex(xs, xs(transmitterIdx) + k, idx + 1)
                _solve(xs, transmitterEndIdx + 1, acc + 1)
            }
        }
        _solve(x.sorted, 0, 0)
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var x = new Array[Int](n);
        for(x_i <- 0 to n-1) {
           x(x_i) = sc.nextInt();
        }
        val result = hackerlandRadioTransmitters(x, k);
        println(result)
    }
}
