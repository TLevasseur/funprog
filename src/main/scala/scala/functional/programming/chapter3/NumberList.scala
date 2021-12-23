package scala.functional.programming.chapter3

object NumberList {

  def zipSum(list: List[Int], list2: List[Int]): List[Int] = {
    List.zipWith(list, list2)(_ + _)
  }

  def removeOddsViaFlatMap(list: List[Int]): List[Int] = {
    List.filterViaFlatMap(list)(_ % 2 > 0)
  }

  def removeOdds(list: List[Int]): List[Int] = {
    List.filter(list)(_ % 2 > 0)
  }

  def addOne(list: List[Int]): List[Int] = {
    List.map(list)(_ + 1)
  }

  def listToString(list: List[Int]): List[String] = {
    List.map(list)(_.toString)
  }


}
