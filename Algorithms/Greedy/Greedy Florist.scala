object Solution {

  def solve(arr: Array[Int], people: Int): Int = {
    def _solve(arr: List[Int], step: Int, x: Int, acc: Int): Int = {
      arr match {
        case Nil =>
          acc
        case y :: ys =>
          if (step + 1 == people) {
            _solve(ys, 0, x + 1, acc + y * (x + 1))
          } else {
            _solve(ys, step + 1, x, acc + y * (x + 1))
          }
      }
    }
    _solve(arr.sortBy(x => -x).toList, 0, 0, 0)
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var c = new Array[Int](n);
        for(c_i <- 0 to n-1) {
           c(c_i) = sc.nextInt();
        }
        println(solve(c, k))
    }
}
