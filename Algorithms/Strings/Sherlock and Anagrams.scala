import scala.collection.immutable.HashMap

object Solution {

    def solve(s: String): Long = {
      val numOfAlphabet = 26

      (0 /: s.indices)((acc, length) => {
        val l = length + 1
        val begin: Vector[Int] = (Array.ofDim[Int](numOfAlphabet).toVector /: (0 until length))((arr, x) => {
          val idx = s(x) - 'a'
          arr.updated(idx, arr(idx) + 1)
        })

        val (hash, _) = ((HashMap[Vector[Int], Int](), begin) /: (length until s.length))((hashAndArray, x) => {
          val arr = hashAndArray._2
          val chrIdx = s(x) - 'a'
          val newArr = if (0 <= x - l) {
            val addedArr = arr.updated(chrIdx, arr(chrIdx) + 1)
            addedArr.updated(s(x - l) - 'a', addedArr(s(x - l) - 'a') - 1)
          } else {
            arr.updated(chrIdx, arr(chrIdx) + 1)
          }
          val newHash = hashAndArray._1.updated(newArr, hashAndArray._1.getOrElse(newArr, 0) + 1)
          (newHash, newArr)
        })

        acc + hash.map(x =>
          x._2 * (x._2 - 1) / 2
        ).sum
      })
    }
    
    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var q = sc.nextInt();
        var a0 = 0;
        while(a0 < q){
            var s = sc.next();
            println(solve(s))
            a0+=1;
        }
    }
}
