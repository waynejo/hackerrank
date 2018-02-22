object Solution {

  def solve(coins: Array[Int], n: Int): Long =  {
    val limit = 250 + 1

    def _solve(coins: Array[Int], prev: Array[Long]): Long = {
      if (coins.isEmpty) {
        prev(n)
      } else {
        val coin = coins.head
        val sums = Array.ofDim[Long](limit)
        (1 to (limit - 1) / coin).foreach(x => sums(x * coin) = 1)
        (0 to limit / coin).foreach(x => {
          val now = x * coin
          (now + 1 until limit).foreach(y => sums(y) = sums(y) + prev(y - now))
        })
        _solve(coins.tail, sums)
      }
    }
    _solve(coins, Array.ofDim[Long](limit))
  }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var m = sc.nextInt();
        var c = new Array[Int](m);
        for(c_i <- 0 to m-1) {
           c(c_i) = sc.nextInt();
        }
        println(solve(c, n))
    }
}
