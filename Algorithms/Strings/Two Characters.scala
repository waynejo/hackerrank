object Solution {

  def twoCharaters(s: String): Int =  {
    val chars = s.toCharArray.distinct
    val lengths = for {
      x <- chars
      y <- chars if x != y
    } yield {
      val newChar = s.toCharArray.filter(c => c == x || c == y)
      if (1 >= newChar.length || newChar.sliding(2).exists(x => x(0) == x(1))) {
        0
      } else {
        newChar.length
      }
    }
    if (lengths.isEmpty) {
      0
    } else {
      lengths.max
    }
  }


  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var l = sc.nextInt();
    var s = sc.next();
    val result = twoCharaters(s);
    println(result)
  }
}
