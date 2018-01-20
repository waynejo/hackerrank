object Solution {

    def digitSum(n: String, k: Int): Int = {
        if (1 == n.length && k == 1) {
            n.toInt
        } else if (1 == n.length) {
            digitSum((BigInt(n) * k).toString, 1)
        } else {
            digitSum((BigInt(0) /: n)((acc, x) => acc + (x - '0')).toString, k)
        }
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.next();
        var k = sc.nextInt();
        val result = digitSum(n, k);
        println(result)
    }
}
