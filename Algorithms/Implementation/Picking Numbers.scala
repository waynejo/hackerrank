object Solution {

  def pickingNumbers(a: Array[Int]): Int = {
    val counts = a.groupBy(x => x).mapValues(x => x.length)
    (0 /: counts.keys)((acc, x) => {
      acc max (counts(x) + counts.getOrElse(x + 1, 0))
    })
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var a = new Array[Int](n);
        for(a_i <- 0 to n-1) {
           a(a_i) = sc.nextInt();
        }
        val result = pickingNumbers(a);
        println(result)
    }
}
