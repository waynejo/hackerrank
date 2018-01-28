object Solution {

    def cipher(k: Int, s: String): String =  {
        def xor(one: Char, another: Char): Char = {
            (one, another) match {
                case ('0', '0') => '0'
                case ('1', '1') => '0'
                case _ => '1'
            }
        }

        val textLength = s.length - k + 1
        (('0', (" " * textLength).toArray) /: (0 until textLength)){ case ((lastAcc, acc), x) =>
            val next = if (0 == x) {
                '0'
            } else if (x >= k) {
                xor(xor(lastAcc, acc(x - 1)), acc(x - k))
            } else {
                xor(lastAcc, acc(x - 1))
            }
            if ('0' == s(x)) {
                acc(x) = next
                (next, acc)
            } else if ('0' == next) {
                acc(x) = '1'
                (next, acc)
            } else {
                acc(x) = '0'
                (next, acc)
            }
        }._2.mkString
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var s = sc.next();
        val result = cipher(k, s);
        println(result)
    }
}
