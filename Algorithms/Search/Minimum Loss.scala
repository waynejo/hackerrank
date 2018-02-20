object Solution {

  def minimumLoss(price: Array[Long]): Long =  {
    val items = price.zipWithIndex.sortBy(-_._1)
    ((items.head._1, items.head) /: items.tail){(acc, x) =>
      val last = acc._2
      val (value, newIdx) = x
      if (newIdx < last._2) {
        (acc._1, x)
      } else {
        ((last._1 - value) min acc._1, x)
      }
    }._1
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var price = new Array[Long](n);
        for(price_i <- 0 to n-1) {
           price(price_i) = sc.nextLong();
        }
        val result = minimumLoss(price);
        println(result)
    }
}
