object Solution {

  def candies(n: Int, arr: Array[Int]): Long =  {
    val counts = Array.ofDim[Long](arr.length)

    (counts /: arr.zipWithIndex.sorted.toList)((acc, x) => {
      val idx = x._2
      val isBiggerThanLeft = 0 != idx && arr(idx - 1) < arr(idx)
      val isBiggerThanRight = arr.length - 1 != idx && arr(idx + 1) < arr(idx)
      if (isBiggerThanLeft && isBiggerThanRight) {
        acc(idx) = (acc(idx - 1) max acc(idx + 1)) + 1
      } else if (isBiggerThanLeft) {
        acc(idx) = acc(idx - 1) + 1
      } else if (isBiggerThanRight) {
        acc(idx) = acc(idx + 1) + 1
      } else {
        acc(idx) = 1
      }
      acc
    })
    counts.sum
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.nextInt();
        }
        val result = candies(n, arr);
        println(result)
    }
}
