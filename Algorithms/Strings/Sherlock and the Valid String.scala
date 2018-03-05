import scala.io.StdIn


object Solution extends App {

  def solve(str: String): Boolean = {
    val counts: Array[Int] = str.groupBy(x => x).map(x => x._2.length).toArray
    val isValidByFirstReason = counts.forall(_ == counts(0))
    val countMap = counts.groupBy(x => x).mapValues(x => x.length)
    val isOnlyOne1 = countMap.contains(1) && countMap(1) == 1
    val isDiff1AndMaxIsOnlyOne = 1 == countMap.keys.max - countMap.keys.min && countMap(countMap.keys.max) == 1
    val isValidBySecondReason = countMap.size == 2 && (isOnlyOne1 || isDiff1AndMaxIsOnlyOne)
    isValidByFirstReason || isValidBySecondReason
  }

  val value = StdIn.readLine()
  if (solve(value)) {
    println("YES")
  } else {
    println("NO")
  }
}