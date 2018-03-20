object Solution {

  def passwordCracker(pass: Array[String], attempt: String): String =  {
    val endIdx = attempt.length
    val isAble = Array.fill(attempt.length)(true)
    def _solve(idx: Int, acc: List[String]): List[String] = {
      if (idx == endIdx) {
        acc.reverse
      } else if (!isAble(idx)) {
        Nil
      } else {
        for (i <- pass.indices) {
          val s = pass(i)
          if (attempt.startsWith(s, idx)) {
            val result = _solve(idx + s.length, s :: acc)
            if (result.nonEmpty) {
              return result
            }
          }
        }
        isAble(idx) = false
        Nil
      }
    }

    _solve(0, Nil) match {
      case Nil =>
        "WRONG PASSWORD"
      case xs =>
        xs.mkString(" ")
    }
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var t = sc.nextInt();
        var a0 = 0;
        while(a0 < t){
            var n = sc.nextInt();
            var pass = new Array[String](n);
            for(pass_i <- 0 to n-1) {
               pass(pass_i) = sc.next();
            }
            var attempt = sc.next();
            val result = passwordCracker(pass, attempt);
            println(result)
            a0+=1;
        }
    }
}
