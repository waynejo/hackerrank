object Solution {

    def angryChildren(k: Int, arr: Array[Int]): Int =  {
        val values = arr.sorted
        (0 until values.length - k + 1).map(idx => values(idx + k - 1) - values(idx)).min
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.nextInt();
        }
        val result = angryChildren(k, arr);
        println(result)
    }
}
