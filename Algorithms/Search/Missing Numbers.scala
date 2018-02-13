object Solution {

    def missingNumbers(arr: Array[Int], brr: Array[Int]): Array[Int] =  {
        def _solve(a: List[Int], b: List[Int], acc: List[Int] = Nil): List[Int] = {
            (a, b) match {
                case (x :: xs, y :: ys) if x == y =>
                    _solve(xs, ys, acc)
                case (_, y :: ys) =>
                    _solve(a, ys, y :: acc)
                case (_, _) =>
                    acc
            }
        }
        _solve(arr.sorted.toList, brr.sorted.toList).reverse.toArray.distinct
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
            arr(arr_i) = sc.nextInt();
        }
        var m = sc.nextInt();
        var brr = new Array[Int](m);
        for(brr_i <- 0 to m-1) {
            brr(brr_i) = sc.nextInt();
        }
        val result = missingNumbers(arr, brr);
        println (result.mkString(" "))


    }
}
