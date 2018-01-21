object Solution {

    def powerSum(X: Int, N: Int): Int = {
        def powerN(x: Int, n: Int): Int = {
            (1 /: (0 until n))((acc, _) => acc * x)
        }

        val values = (0 to X + 1).map(x => powerN(x, N)).toArray
        val counts = Array.fill[Long](X + 1)(0)

        val exceedIndex = values.indexWhere(_ > 1000)
        val endIdx = if (-1 == exceedIndex) X else exceedIndex min X
        for (i <- 1 to endIdx) {
            for (k <- X - values(i) to 0 by -1) {
                var dstIdx = values(i) + k
                counts(dstIdx) = counts(dstIdx) + counts(k)
            }
            if (values(i) <= X) {
                counts(values(i)) = counts(values(i)) + 1
            }
        }
        counts(X).toInt
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var X = sc.nextInt();
        var N = sc.nextInt();
        val result = powerSum(X, N);
        println(result)
    }
}
