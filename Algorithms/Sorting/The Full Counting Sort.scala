import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

object Solution {

    def main(args: Array[String]) {
        def update(values: Array[mutable.Queue[String]], x: Int, s: String) {
            values(x).enqueue(s)
        }

        @tailrec
        def printText(values: Array[mutable.Queue[String]], idx: Int, isFirst: Boolean = true) {
            if (idx < values.length) {
                if (values(idx).isEmpty) {
                    printText(values, idx + 1, isFirst = false)
                } else {
                    if (isFirst) {
                        print(values(idx).dequeue().substring(1))
                    } else {
                        print(values(idx).dequeue())
                    }
                    printText(values, idx, isFirst = false)
                }
            }
        }

        val values = Array.fill[mutable.Queue[String]](100)(mutable.Queue[String]())

        val n = StdIn.readLine().toInt
        (0 until n).foreach(idx => {
            val Array(x, s) = StdIn.readLine().split(" ")
            update(values, x.toInt, if (idx < n / 2) " -" else " " + s)
        })

        printText(values, 0)
    }
}
