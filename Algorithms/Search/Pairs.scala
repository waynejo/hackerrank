object Solution {

    def idxOf(arr: Array[Int], v: Int, start: Int, end: Int, isRightMost: Boolean): Option[Int] =  {
        if (start + 1 >= end) {
            if (start < end && arr(start) == v) {
                Some(start)
            } else {
                None
            }
        } else {
            val mid = (start + end) / 2
            if (arr(mid) == v) {
                if (isRightMost) {
                    idxOf(arr, v, mid, end, isRightMost)
                } else if (arr(mid - 1) == v) {
                    idxOf(arr, v, start, mid, isRightMost)
                } else {
                    Some(mid)
                }
            } else if (arr(mid) < v) {
                idxOf(arr, v, mid + 1, end, isRightMost)
            } else {
                idxOf(arr, v, start, mid, isRightMost)
            }
        }
    }

    def pairs(k: Int, arr: Array[Int]): Int =  {
        val sortedArray = arr.sorted
        val endOfIdx = sortedArray.length
        (0 /: sortedArray.indices)((acc, x) => {
            val count = for {
                rIdx <- idxOf(sortedArray, sortedArray(x) + k, x, endOfIdx, isRightMost = true)
                lIdx <- idxOf(sortedArray, sortedArray(x) + k, x, endOfIdx, isRightMost = false)
            } yield rIdx - lIdx + 1
            acc + count.getOrElse(0)
        })
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var k = sc.nextInt();
        var arr = new Array[Int](n);
        for(arr_i <- 0 to n-1) {
           arr(arr_i) = sc.nextInt();
        }
        val result = pairs(k, arr);
        println(result)
    }
}
