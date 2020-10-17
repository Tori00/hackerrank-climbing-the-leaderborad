import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Solution {

  @tailrec
  def findRank(distinctRank: List[(Int, Int)], player: List[Int], solution: ListBuffer[Int]): ListBuffer[Int] = {
    if (player.isEmpty) solution
    else if (distinctRank.isEmpty && player.nonEmpty) {
      player.indices.foreach(_ => solution += 1)
      solution
    }
    else {
      val currentPoint = player.head

      val remainingRanks = distinctRank.dropWhile(_._1 < currentPoint)

      val currentSolution = {
        if (remainingRanks.isEmpty) 0
        else {
          val head = remainingRanks.head
          val headRank = head._2

          if (head._1 == currentPoint) headRank else headRank + 1
        }
      }

      findRank(remainingRanks, player.tail, solution += (currentSolution + 1)) // +1, because indexing start with 0
    }
  }

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
    val reversedDistinctPoints = ranked.distinct.zipWithIndex.reverse.toList
    val solution = new ListBuffer[Int]()

    findRank(reversedDistinctPoints, player.toList, solution).toArray
  }

  def main(args: Array[String]): Unit = {
    val result = climbingLeaderboard(Array(100, 100, 50, 40, 40, 20, 10), Array(5, 25, 50, 120))
    println(result.mkString(","))

    val expected = Array(6, 4, 2, 1)
    println(s"Is solution correct? ${result sameElements expected}")
  }
}
