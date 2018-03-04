object Solution {

    def pokerNim(k: Int, c: Array[Int]): String =  {
        val nimSums = (0 /: c)((acc, x) => acc ^ x)
        if (0 == nimSums) {
            "Second"
        } else {
            "First"
        }
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextInt();
            var k = sc.nextInt();
            var c = new Array[Int](n);
            for(c_i <- 0 to n-1) {
               c(c_i) = sc.nextInt();
            }
            val result = pokerNim(k, c);
            println(result)
            a0+=1;
        }
    }
}
