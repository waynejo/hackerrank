object Solution {
    def convertSearchedIndex(idx: Int): Int = {
        if (-1 == idx) {
            Int.MaxValue
        } else {
            idx
        }
    }

    def bigSorting(arr: Array[String]): Array[String] =  {
        arr.sortWith((x, y) => {
            if (x.length == y.length) {
                val lIdx = (x zip y).indexWhere(x => x._1 > x._2)
                val rIdx = (x zip y).indexWhere(x => x._1 < x._2)
                convertSearchedIndex(lIdx) > convertSearchedIndex(rIdx)
            } else {
                x.length < y.length
            }
        })
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var arr = new Array[String](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.next();
        }
        val result = bigSorting(arr);
        println (result.mkString("\n"))


    }
}
