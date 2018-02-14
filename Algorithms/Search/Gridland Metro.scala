object Solution {

    def gridlandMetro(n: Long, m: Long, k: Int, track: Array[Array[Long]]): Long =  {
        def merge(input: Array[Array[Long]], index: Int, acc: List[(Long, Long)]): List[(Long, Long)] = {
            if (input.length <= index) {
                acc
            } else {
                val now = input(index)
                if (acc.head._2 < now(1)) {
                    merge(input, index + 1, (now(1), now(2)) :: acc)
                } else {
                    merge(input, index + 1, (acc.head._1, acc.head._2 max now(2)) :: acc.tail)
                }
            }
        }

        val numOfFilledTile = track.groupBy(x => x(0)).map{case ((_: Long, x: Array[Array[Long]])) =>
            val sorted = x.map(y => Array(y(0), y(1) min y(2), y(1) max y(2))).sortBy(y => y(1))
            merge(sorted.tail, 0, (sorted.head(1), sorted.head(2)) :: Nil).map(x => x._2 - x._1 + 1).sum
        }.sum

        n * m - numOfFilledTile
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var m = sc.nextInt();
        var k = sc.nextInt();
        var track = Array.ofDim[Long](k,3);
        for(track_i <- 0 to k-1) {
            for(track_j <- 0 to 3-1){
                track(track_i)(track_j) = sc.nextLong();
            }
        }
        val result = gridlandMetro(n, m, k, track);
        println(result)
    }
}
