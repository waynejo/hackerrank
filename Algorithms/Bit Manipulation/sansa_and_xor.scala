object Solution {

  def sansaXor(arr: Array[Int]): Int =  {
    if (0 == arr.length % 2) {
      0
    } else {
      (0 /: arr.indices)((acc, idx) => {
        if (0 == idx % 2) {
          acc ^ arr(idx)
        } else {
          acc
        }
      })
    }
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
            val result = sansaXor(arr);
            println(result)
            a0+=1;
        }
    }
}
