object Solution {

    def icecreamParlor(m: Int, arr: Array[Int]): Array[Int] =  {
        val result = (for {
            i <- arr.indices
            k <- i + 1 until arr.length
            price = arr(i) + arr(k) if price <= m
        } yield (i, k, price)).maxBy(x => x._3)
        Array(result._1 + 1, result._2 + 1)
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var m = sc.nextInt();
            var n = sc.nextInt();
            var arr = new Array[Int](n);
            for(arr_i <- 0 to n-1) {
                arr(arr_i) = sc.nextInt();
            }
            val result = icecreamParlor(m, arr);
            println (result.mkString(" "))


            a0+=1;
        }
    }
}
