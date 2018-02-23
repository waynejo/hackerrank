object Solution {

  def isAndyWin(numbers: Array[Int]): Boolean =  {
    val (count, _) = ((0, 0) /: numbers)((acc, x) =>
      if (acc._2 < x) {
        (acc._1 + 1, x)
      } else {
        acc
      }
    )
    0 == count % 2
  }

    
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    var g = sc.nextInt()
    var a0 = 0

    while(a0 < g){
      val qNum = sc.nextInt()
      val inputs = (0 until qNum).map(_ => sc.nextInt()).toArray
      println(if (isAndyWin(inputs)) "ANDY" else "BOB")
      a0+=1
    }
  }
}
