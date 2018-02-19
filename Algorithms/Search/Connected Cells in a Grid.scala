import scala.annotation.tailrec

object Solution {

  def connectedCell(matrix: Array[Array[Int]]): Int =  {
    val n = matrix.length
    val m = matrix.head.length
      
    def traverse(x: Int, y: Int): Int = {
      matrix(y)(x) = 0

      val neighborSize = for {
        dy <- -1 to 1
        dx <- -1 to 1
      } yield {
        val newX = x + dx
        val newY = y + dy
        if (0 <= newY && n > newY && 0 <= newX && m > newX && matrix(newY)(newX) == 1) {
          matrix(newY)(newX) = 0
          traverse(newX, newY)
        } else {
          0
        }
      }
      neighborSize.sum + 1
    }

    @tailrec
    def calc(acc: Int = 0): Int = {
      val indexedNumbers = for {
        (arr, y) <- matrix.zipWithIndex
        (value, x) <- arr.zipWithIndex if value == 1
      } yield (x, y, value)

      indexedNumbers.headOption match {
        case Some((x, y, _)) =>
          calc(acc max traverse(x, y))
        case _ =>
          acc
      }
    }
    calc()
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var m = sc.nextInt();
        var matrix = Array.ofDim[Int](n,m);
        for(matrix_i <- 0 to n-1) {
           for(matrix_j <- 0 to m-1){
              matrix(matrix_i)(matrix_j) = sc.nextInt();
           }
        }
        val result = connectedCell(matrix);
        println(result)
    }
}
