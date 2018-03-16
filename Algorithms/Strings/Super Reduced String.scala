object Solution {
  def solve(s: String): String = {
    val result = ("" /: s)((acc, x) => if (acc.isEmpty) {
      x.toString
    } else if (acc.last == x) {
      acc.init
    } else {
      acc + x
    })
    if (result != s) {
      solve(result)
    } else {
      if (result.isEmpty) {
        "Empty String"
      } else {
        result
      }
    }
  }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var s = sc.next();
        println(solve(s))
    }
}
