object Solution {

  def flippingMatrix(matrix: Array[Array[Int]]): Int =  {
    val n = matrix.length
    val values = for {
      i <- 0 until n / 2
      k <- 0 until n / 2
    } yield matrix(i)(k) max matrix(i)(n - k - 1) max matrix(n - i - 1)(k) max matrix(n - i - 1)(n - k - 1)
    values.sum
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var q = sc.nextInt();
        var a0 = 0;
        while(a0 < q){
            var n = sc.nextInt();
            var matrix = Array.ofDim[Int](2*n,2*n);
            for(matrix_i <- 0 to 2*n-1) {
               for(matrix_j <- 0 to 2*n-1){
                  matrix(matrix_i)(matrix_j) = sc.nextInt();
               }
            }
            val result = flippingMatrix(matrix);
            println(result)
            a0+=1;
        }
    }
}
