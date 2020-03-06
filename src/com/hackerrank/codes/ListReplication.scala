package com.hackerrank.codes

/**
 * @author sahilgogna on 2020-03-02
 */
object ListReplication extends App {

  //repeating an element for n times and printing a list
  def f(num:Int,list:List[Int]):List[Int] = list.flatMap(List.fill(num)(_))
  println(f(3,List(1,2,3)))

  // reversing a list
  def f(arr:List[Int]):List[Int] = {
    arr match {
      case head:: Nil => List(head)
      case head :: tail => f(tail) :+ head
    }
  }

  // printing sum of all odd element
  def f2(arr:List[Int]):Int = {
    val ele: List[Int] = arr.filter(_ %2 != 0)
    ele.sum
  }
  println(f2(List(1,2,3,4)))

  // writing your own filter method to filter values less than a certain number from the list
}
