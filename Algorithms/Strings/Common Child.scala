object Solution {
  def solve(text1: String, text2: String): Int =  {

    def _solve(idx1: Int, idx2: Int, previous: Array[Int], now: Array[Int]): Int = {
      if (idx1 == text1.length) {
        previous.last
      } else if (idx2 == text2.length){
        _solve(idx1 + 1, 0, now, Array.ofDim[Int](text1.length))
      } else {
        val (lValue, ltValue) = if (0 == idx2) {
          (0, 0)
        } else {
          (now(idx2 - 1), previous(idx2 - 1))
        }
        val tValue = previous(idx2)
        val value = if (text1.charAt(idx1) == text2.charAt(idx2)) {
          ltValue + 1
        } else {
          lValue max tValue
        }
        now(idx2) = value
        _solve(idx1, idx2 + 1, previous, now)
      }
    }

    _solve(0, 0, Array.ofDim[Int](text1.length), Array.ofDim[Int](text1.length))
  }
    
  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var s1 = sc.next();
    var s2 = sc.next();
    println(solve(s1, s2))
  }
}
