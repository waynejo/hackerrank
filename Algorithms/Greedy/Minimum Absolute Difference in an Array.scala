object Solution {

  def minimumAbsoluteDifference(n: Int, arr: Array[Int]): Int =  {
    val sorted = arr.sorted
    (Int.MaxValue /: (0 until (arr.length - 1)))((acc, x) => acc min (sorted(x + 1) - sorted(x)))
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.nextInt();
        }
        val result = minimumAbsoluteDifference(n, arr);
        println(result)
    }
}