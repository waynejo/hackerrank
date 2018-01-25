object Solution {

    def maximumToys(prices: Array[Int], k: Int): Int =  {
        ((0, 0) /:prices.sorted){case ((acc, count), x) =>
                if (acc + x <= k) {
                    (acc + x, count + 1)
                } else {
                    (acc, count)
                }
        }._2
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var prices = new Array[Int](n);
        for(prices_i <- 0 to n-1) {
           prices(prices_i) = sc.nextInt();
        }
        val result = maximumToys(prices, k);
        println(result)
    }
}
