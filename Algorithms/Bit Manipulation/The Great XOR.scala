import scala.io.StdIn

object Solution extends App {

  def solve(value: Long, level: Long = 1, acc: Long = 0): Long = {
    if (0 == value) {
      acc
    } else if (0 == (value & 1)) {
      solve(value >> 1, level << 1, acc + level)
    } else {
      solve(value >> 1, level << 1, acc)
    }
  }

  val n = StdIn.readInt()
  (0 until n).foreach(i => {
    val value = StdIn.readLong()
    println(solve(value))
  })
}
