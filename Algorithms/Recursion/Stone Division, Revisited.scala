import scala.collection.immutable.{HashMap, Queue}

object Solution {

  def stoneDivision(n: Long, s: Array[Long]): Long =  {
    def solve(acc: HashMap[(Long, Long), Long], queue: Queue[(Long, Long, Long)]): Long = {
      queue match {
        case (number, count, answer) +: remain =>
          val (newAcc, newQueue) = ((acc, remain) /: s)((accAndQueue, x) => {
            val (acc, queue) = accAndQueue
            if (0 == number % x && number > x) {
              val newCount = count * (number / x)
              val newAnswer = answer + count
              if (acc.contains((x, newCount)) && acc((x, newCount)) > newAnswer) {
                accAndQueue
              } else {
                (acc + ((x, newCount) -> newAnswer), queue :+ (x, newCount, newAnswer))
              }
            } else {
              accAndQueue
            }
          })
          solve(newAcc, newQueue)
        case _=>
          if (acc.isEmpty) {
            0
          } else {
            acc.maxBy(_._2)._2
          }
      }
    }
    solve(HashMap[(Long, Long), Long](), Queue[(Long, Long, Long)]((n, 1, 0)))
  }
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var q = sc.nextInt();
        var a0 = 0;
        while(a0 < q){
            var n = sc.nextLong();
            var m = sc.nextInt();
            var s = new Array[Long](m);
            for(s_i <- 0 to m-1) {
               s(s_i) = sc.nextLong();
            }
            val result = stoneDivision(n, s);
            println(result)
            a0+=1;
        }
    }
}
