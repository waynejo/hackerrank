object Solution {

  def luckBalance(n: Int, k: Int, contests: Array[Array[Int]]): Int =  {
    val totalLuck = contests.map(_(0)).sum
    val importantContests = contests.filter(_ (1) == 1).map(_ (0)).sorted
    val lostLuck = importantContests.take(importantContests.length - k).sum
    totalLuck - lostLuck * 2
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var contests = Array.ofDim[Int](n,2);
        for(contests_i <- 0 to n-1) {
           for(contests_j <- 0 to 2-1){
              contests(contests_i)(contests_j) = sc.nextInt();
           }
        }
        val result = luckBalance(n, k, contests);
        println(result)
    }
}
