object Solution {

  def marsExploration(s: String): Int =  {
    val answer = "SOS" * (s.length  / 3)
    (answer zip s).count(x => x._1 != x._2)
  }


    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var s = sc.next();
        val result = marsExploration(s);
        println(result)
    }
}
