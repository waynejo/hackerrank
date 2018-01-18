
object Solution {

    def substrings(balls: String): Int =  {
        val remainder = 1000000007

        ((0L, 1L) /: (balls.length to 1 by -1)) {
            case ((acc, exp), x) =>
                ((acc + x * exp * (balls(x - 1).toInt - '0')) % remainder, (exp * 10 + 1) % remainder)
        }._1.toInt
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var balls = sc.next();
        val result = substrings(balls);
        println(result)
    }
}
