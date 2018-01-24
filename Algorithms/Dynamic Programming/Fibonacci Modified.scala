object Solution {

    def fibonacciModified(t1: BigInt, t2: BigInt, n: Int): BigInt = {
        if (1 == n) {
            t1
        } else if (2 == n) {
            t2
        } else {
            fibonacciModified(t2, t1 + t2 * t2, n - 1)
        }
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t1 = sc.nextInt();
        var t2 = sc.nextInt();
        var n = sc.nextInt();
        val result = fibonacciModified(t1, t2, n);
        println(result)
    }
}
