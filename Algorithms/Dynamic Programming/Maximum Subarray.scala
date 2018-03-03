
import scala.io.StdIn

object Solution extends App {

  def solve(values: Array[Int]): Array[Int] = {
    def _solve(idx: Int, acc: Int = 0, answer: Int = 0): Int = {
      if (idx == values.length) {
        answer
      } else {
        if (acc + values(idx) < values(idx)) {
          _solve(idx + 1, values(idx), values(idx) max answer)
        } else {
          _solve(idx + 1, acc + values(idx), (acc + values(idx)) max answer)
        }
      }
    }
    Array(
      _solve(0, 0, values(0)),
      if (!values.exists(_ >= 0)) values.max else values.filter(_ > 0).sum
    )
  }

  val n = StdIn.readInt()
  (0 until n).foreach(i => {
    val _ = StdIn.readInt()

    val values = StdIn.readLine().split(" ").map(_.toInt)
    println(solve(values).mkString(" "))
  })
}
