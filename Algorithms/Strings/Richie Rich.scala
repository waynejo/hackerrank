object Solution {

    def solve(k: Int, s: String): String = {
        val mid = (s.length - 1) / 2
        val diffNum = (0 to mid).count(x => s(x) != s(s.length - x - 1))
        val remains = k - diffNum

        def _solve(diffNum: Int, remains: Int, x: Int, text: Array[Char]): String = {
            val mirroredX = s.length - x - 1
            if (x > mid) {
                text.mkString
            } else {
                val diffNumberCount = if (s(x) != s(mirroredX)) 1 else 0
                val notNineCount = Array(x, mirroredX).distinct.filter(x => 0 <= x && x < s.length).map(s).count(_ != '9')
                if (notNineCount - diffNumberCount <= remains) {
                    text(x) = '9'
                    text(mirroredX) = '9'
                    _solve(diffNum - diffNumberCount, remains - notNineCount + diffNumberCount, x + 1, text)
                } else if (s(x) != s(mirroredX)) {
                    if (s(x) > s(mirroredX)) {
                        text(mirroredX) = s(x)
                        _solve(diffNum - 1, remains, x + 1, text)
                    } else {
                        text(x) = s(mirroredX)
                        _solve(diffNum - 1, remains, x + 1, text)
                    }
                } else {
                    _solve(diffNum, remains, x + 1, text)
                }
            }
        }
        if (0 > remains) {
            "-1"
        } else {
            _solve(diffNum, remains, 0, s.toCharArray)
        }
    }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var s = sc.next();
        
        println(solve(k, s))
    }
}
