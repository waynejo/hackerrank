object Solution {

    def equal(arr: Array[Int]): Int =  {
        def _solve(minimum: Int): Int = {
            arr.map { x =>
                val no5 = (x - minimum) / 5
                val no2 = (x - minimum - no5 * 5) / 2
                val no1 = x - minimum - no5 * 5 - no2 * 2
                no5 + no2 + no1
            }.sum
        }
        val minimum = arr.min
        _solve(minimum) min _solve(minimum - 1) min _solve(minimum - 2) min _solve(minimum - 3) min _solve(minimum - 4)
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextInt();
            var arr = new Array[Int](n);
            for(arr_i <- 0 to n-1) {
                arr(arr_i) = sc.nextInt();
            }
            val result = equal(arr);
            println(result)
            a0+=1;
        }
    }
}
