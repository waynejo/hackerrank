import scala.io.StdIn

object Solution extends App {

  val MAX_INPUT = 1000000
  val isPrime:Array[Boolean] = Array.fill(MAX_INPUT + 1)(true)
  for (i <- 2 to MAX_INPUT) {
    if (isPrime(i)) {
      for (k <- 2 * i to MAX_INPUT by i) {
        isPrime(k) = false
      }
    }
  }

  def solve(n: Int): String = {
    val isAliceWin = 0 == (1 to n).count(x => isPrime(x)) % 2
    if (isAliceWin) {
      "Alice"
    } else {
      "Bob"
    }
  }

  val g = StdIn.readInt()
  for (i <- 0 until g) {
    val n = StdIn.readInt()
    println(solve(n))
  }
}
