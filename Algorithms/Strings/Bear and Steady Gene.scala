object Solution {

  def steadyGene(gene: String): Int = {
    val charArray = gene.toCharArray
    val optimalNum = gene.length / 4
    val overCount = Array(
      (charArray.count(_ == 'A') - optimalNum) max 0,
      (charArray.count(_ == 'T') - optimalNum) max 0,
      (charArray.count(_ == 'C') - optimalNum) max 0,
      (charArray.count(_ == 'G') - optimalNum) max 0
    )
    def indexOfCount(c: Char): Int = {
      c match {
        case 'A' => 0
        case 'T' => 1
        case 'C' => 2
        case _ => 3
      }
    }
    def findMinSubStringLength(begin: Int, end: Int, counts: Array[Int], acc: Int): Int = {
      val isModifiable = (counts zip overCount).forall(x => x._1 >= x._2)
      val nextAcc = if (isModifiable) {
        (end - begin) min acc
      } else {
        acc
      }
      if (isModifiable && begin < end) {
        val index = indexOfCount(gene(begin))
        counts(index) = counts(index) - 1
        findMinSubStringLength(begin + 1, end, counts, nextAcc)
      } else if (end == gene.length) {
        acc
      } else {
        val index = indexOfCount(gene(end))
        counts(index) = counts(index) + 1
        findMinSubStringLength(begin, end + 1, counts, nextAcc)
      }
    }
    if (gene.isEmpty) {
      0
    } else {
      findMinSubStringLength(0, 0, Array.ofDim[Int](4), gene.length)
    }
  }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var gene = sc.next();
        val result = steadyGene(gene);
        println(result)
    }
}
