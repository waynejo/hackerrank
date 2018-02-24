object Solution {

  def minimumBribes(q: Array[Int]): Option[Int] =  {
    def _solve(arr: Array[Int], idx: Int = 0, acc: Int = 0): Option[Int] = {
      if (idx == arr.length) {
        Some(acc)
      } else if (arr(idx) == q(idx)) {
        _solve(arr, idx + 1, acc)
      } else if (idx + 1 < arr.length && arr(idx + 1) == q(idx)) {
        arr(idx + 1) = arr(idx)
        arr(idx) = q(idx)
        _solve(arr, idx + 1, acc + 1)
      } else if (idx + 2 < arr.length && arr(idx + 2) == q(idx)) {
        arr(idx + 2) = arr(idx + 1)
        arr(idx + 1) = arr(idx)
        arr(idx) = q(idx)
        _solve(arr, idx + 1, acc + 2)
      } else {
        None
      }
    }
    _solve((1 to q.length).toArray)
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextInt();
            var q = new Array[Int](n);
            for(q_i <- 0 to n-1) {
               q(q_i) = sc.nextInt();
            }
            minimumBribes(q) match {
            case Some(x) =>
                println(x)
            case None =>
                println("Too chaotic")
            }
            a0+=1;
        }
    }
}