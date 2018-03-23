object Solution {

  def counterGame(n: Long): String =  {
    def isPowerOfTwo(n: Long, countOfOne: Int = 0, acc: Long = 0): (Boolean, Long) = {
      val nextAcc = if (0 == acc) {
        1
      } else {
        acc << 1
      }
      if (0 == n) {
        (1 >= countOfOne, acc)
      } else if ((n & 1) == 1) {
        isPowerOfTwo(n >> 1, countOfOne + 1, nextAcc)
      } else {
        isPowerOfTwo(n >> 1, countOfOne, nextAcc)
      }
    }
    def _solve(n: Long, isRichard: Boolean): Boolean = {
      val (isPower, pow) = isPowerOfTwo(n)
      if (1 == n) {
        !isRichard
      } else if (isPower) {
        _solve(n >> 1, !isRichard)
      } else {
        _solve(n - pow, !isRichard)
      }
    }
    if (_solve(n, false)) {
      "Richard"
    } else {
      "Louise"
    }
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextLong();
            val result = counterGame(n);
            println(result)
            a0+=1;
        }
    }
}
