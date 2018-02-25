object Solution {

  def beautifulPairs(a: Array[Int], b: Array[Int]): Int =  {

    def _solve(a: List[Int], b: List[Int], acc: Int = 0, diff: Int = 0): Int = {
      (a, b) match {
        case (_, Nil) | (Nil, _) => if (0 < diff) acc + 1 else acc - 1
        case (x :: xs, y :: ys) if x == y => _solve(xs, ys, acc + 1, diff)
        case (x :: xs, y :: ys) if x < y => _solve(xs, b, acc, diff + 1)
        case (_ :: xs, _ :: ys) => _solve(a, ys, acc, diff + 1)
      }
    }
    _solve(a.sorted.toList, b.sorted.toList)
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var A = new Array[Int](n);
        for(A_i <- 0 to n-1) {
           A(A_i) = sc.nextInt();
        }
        var B = new Array[Int](n);
        for(B_i <- 0 to n-1) {
           B(B_i) = sc.nextInt();
        }
        val result = beautifulPairs(A, B);
        println(result)
    }
}