import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Solution {

    case class Node(var parent: Int, value: Int, var sum: Long, var children: List[Int])

    @tailrec
    def updateNode(data: Array[Node], idx: Int, value: Long) {
        data(idx).sum = data(idx).sum + value
        data(idx).parent match {
            case x if x >= 0 => updateNode(data, x, value)
            case _ =>
        }
    }

    @tailrec
    def linkTree(data: Array[Node], indexes: Queue[Int], edges: Map[Int, Array[Int]]): Array[Node] =  {
        indexes match {
            case x +: xs =>
                val links = edges(x)
                var next = List[Int]()
                links.foreach { link =>
                    if (-1 == data(link).parent && 0 != link) {
                        data(link).parent = x
                        data(x).children = data(x).children :+ link
                        //                        updateNode(data, x, data(link).sum)
                        next = next :+ link
                    }
                }
                linkTree(data, xs ++ next, edges - x)
            case _ =>
                data
        }
    }

    @tailrec
    def updateSums(data: Array[Node], indexes: Queue[Int], acc: List[Int]) {
        indexes match {
            case x +: xs =>
                updateSums(data, xs ++ data(x).children, x :: acc)
            case _ =>
                acc.foreach(idx => data(idx).sum = data(idx).children.map(x => data(x).sum).sum + data(idx).value)
        }
    }

    def cutTheTree(data: Array[Int], edges: Array[Array[Int]]): Int =  {
        val edgeIndexes = edges.map(x => Array(x(0) - 1, x(1) - 1))
        val edgeMap = (edgeIndexes.map(x => (x(0), x(1))) ++ edgeIndexes.map(x => (x(1), x(0)))).groupBy(_._1).mapValues(_.map(_._2))
        val nodes = linkTree(data.map(x => Node(-1, x, x, Nil)), Queue(0), edgeMap)
        updateSums(nodes, Queue(0), Nil)
        val total = nodes(0).sum
        nodes.tail.map(x => math.abs(total - x.sum * 2)).min.toInt
    }

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var n = sc.nextInt();
        var data = new Array[Int](n);
        for(data_i <- 0 to n-1) {
            data(data_i) = sc.nextInt();
        }
        var edges = Array.ofDim[Int](n-1,2);
        for(edges_i <- 0 to n-1-1) {
            for(edges_j <- 0 to 2-1){
                edges(edges_i)(edges_j) = sc.nextInt();
            }
        }
        val result = cutTheTree(data, edges);
        println(result)
    }
}
