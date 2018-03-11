object Solution {

    def shortPalindrome(s: String): Int = {
      val numOfAlphabet = 26
      val numOfA = Array.ofDim[Long](numOfAlphabet)
      val numOfB = Array.ofDim[Long](numOfAlphabet, numOfAlphabet)
      val numOfC = Array.ofDim[Long](numOfAlphabet)

      def solve(idx: Int, numOfA: Array[Long], numOfB: Array[Array[Long]], numOfC: Array[Long], acc: Int): Int = {
        if (idx == s.length) {
          acc
        } else {
          val char = s(idx) - 'a'
          val nextAcc = ((acc + numOfC(char)) % 1000000007).toInt
          (0 until numOfAlphabet).foreach(x => numOfC(x) = numOfC(x) + numOfB(x)(char))
          (0 until numOfAlphabet).foreach(x => numOfB(x)(char) = numOfB(x)(char) + numOfA(x))
          numOfA(char) = numOfA(char) + 1
          solve(idx + 1, numOfA, numOfB, numOfC, nextAcc)
        }
      }

      solve(0, numOfA, numOfB, numOfC, 0)
    }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var s = sc.next();
        val result = shortPalindrome(s);
        println(result)
    }
}
