object Solution {

  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] =  {
    val ranks = Array.ofDim[Int](scores.length)
    scores.indices.foreach((idx) => {
      if (idx == 0) {
        ranks(idx) = 1
      } else if (scores(idx) == scores(idx - 1)) {
        ranks(idx) = ranks(idx - 1)
      } else {
        ranks(idx) = ranks(idx - 1) + 1
      }
    })

    def getRank(left: Int, right: Int, score: Int): Int = {
      if (left == right || left + 1 == right) {
        if (scores(left) <= score) {
          ranks(left)
        } else {
          ranks(left) + 1
        }
      } else {
        val idx = (right + left) / 2
        if (score > scores(idx)) {
          getRank(left, idx, score)
        } else if (score == scores(idx)) {
          ranks(idx)
        } else {
          getRank(idx, right, score)
        }
      }
    }
    alice.map(getRank(0, scores.length, _))
  }
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var scores = new Array[Int](n);
        for(scores_i <- 0 to n-1) {
           scores(scores_i) = sc.nextInt();
        }
        var m = sc.nextInt();
        var alice = new Array[Int](m);
        for(alice_i <- 0 to m-1) {
           alice(alice_i) = sc.nextInt();
        }
        val result = climbingLeaderboard(scores, alice);
        println (result.mkString("\n"))


    }
}
