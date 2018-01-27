object Solution {

    def larrysArray(A: Array[Int]): String = {
        val matches = for {
            x <- A.indices
            y <- x + 1 until A.length
        } yield if (A(x) > A(y)) 1 else 0
        if (0 == matches.sum % 2) "YES" else "NO"
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextInt();
            var A = new Array[Int](n);
            for(A_i <- 0 to n-1) {
               A(A_i) = sc.nextInt();
            }
            val result = larrysArray(A);
            println(result)
            a0+=1;
        }
    }
}
