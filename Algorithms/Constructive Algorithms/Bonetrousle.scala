import scala.io.StdIn


object Solution {

  def solve(n: Long, k: Long, b: Long): Array[Long] = {
    val minSum = b * (b + 1) / 2
    val sum = minSum + BigInt(b) * BigInt(k - b)
    if (minSum > n || sum < n || k < b) {
      Array(-1)
    } else {
      def _solve(acc: Array[Long], sum: BigInt, idx: Int = 0): Array[Long] = {
        if (sum == n) {
          acc
        } else {
          val prev = acc(idx)
          acc(idx) = ((acc(idx) - (sum - n)) max (idx + 1)).toLong
          _solve(acc, sum - (prev - acc(idx)), idx + 1)
        }
      }
      _solve(((k - b + 1) to k).toArray, sum)
    }
  }
    
    def main(args: Array[String]) {
      val n = StdIn.readInt()
      (0 until n).foreach(i => {
        val Array(n, k, b) = StdIn.readLine().split(" ").map(_.toLong)
        println(solve(n, k, b).mkString(" "))
      })
    }
}