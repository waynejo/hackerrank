object Solution {

  def pylons(k: Int, arr: Array[Int]): Int =  {
    def _solve(idx: Int, lowerBound: Int, acc: Int): Int = {
      if (idx >= arr.length) {
        acc
      } else {
        ((idx + k - 1 min arr.length - 1) to lowerBound by -1).find(arr(_) == 1) match {
          case Some(v) =>
            _solve(v + k, v + 1, acc + 1)
          case None =>
            -1
        }
      }
    }
    _solve(0, 0, 0)
  }


    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.nextInt();
        }
        val result = pylons(k, arr);
        println(result)
    }
}
