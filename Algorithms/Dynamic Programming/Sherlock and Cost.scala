object Solution {

    def cost(arr: Array[Int]): Int =  {
        def _solve(idx: Int, left: Int, leftSum: Int, right: Int, rightSum: Int): Int = {
            if (arr.length <= idx) {
                leftSum max rightSum
            } else {
                val newLSum = (leftSum + math.abs(left - 1)) max (rightSum + math.abs(right - 1))
                val newRight = arr(idx)
                val newRSum = (leftSum + math.abs(left - newRight)) max (rightSum + math.abs(right - newRight))
                _solve(idx + 1, 1, newLSum, newRight, newRSum)
            }
        }
        _solve(1, 1, 0, arr.head, 0)
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
            val result = cost(arr);
            println(result)
            a0+=1;
        }
    }
}
