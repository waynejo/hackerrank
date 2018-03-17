object Solution {

  def camelcase(s: String): Int =  {
    s.count(x => 'A' <= x && x <= 'Z') + 1
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var s = sc.next();
        val result = camelcase(s);
        println(result)
    }
}
