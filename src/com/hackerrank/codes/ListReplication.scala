package com.hackerrank.codes

/**
 * @author sahilgogna on 2020-03-02
 */
object ListReplication extends App {

  //repeating an element for n times and printing a list
  def f(num:Int,list:List[Int]):List[Int] = list.flatMap(List.fill(num)(_))
//  println(f(3,List(1,2,3)))

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
//  println(f2(List(1,2,3,4)))

  // writing your own filter method to filter values less than a certain number from the list
  def f3(delim:Int,arr:List[Int]):List[Int] = {
    arr.filter( _< delim)
  }

  def f4(delim:Int,arr:List[Int]):List[Int] = {
    arr match {
      case head::Nil =>if(head < delim) List(head) else Nil
      case head:: tail =>if(head<delim) List(head):::f4(delim,tail) else Nil:::f4(delim,tail)
    }
  }

//  println(f4(6, List(1,2,3,4,5,6,7,8)))


// printing all the odd indexes of the list
  def f5(arr:List[Int]):List[Int] = {
    arr.indices.collect{
      case i if i%2 ==1 => arr(i)
    }.toList
  }
//  println(f5(List(2,5,3,4,6,7,9,8)))

  // crating an array of certain length with all the elements as 1
  def f6(num:Int) : List[Int] = {
    def f1(num:Int,l:List[Int]) : List[Int] = {
      if(num == 0) l
      else f1(num-1, l.appended(1))
    }
    val list = f1(num, List())
    list
  }
//  println(f6(7))

  // counting length of a list
  def f8(arr:List[Int]):Int = {
    arr.map(_=>1).length
  }

  // calculating e^x
  def eulerNumberCalc(x:Int):Double = {
    def innerFun(x:Int, index:Int, sum:Double):Double = {
      if(index == 10) sum
      else {
        val calc = scala.math.pow(x,index)/factorial(index)
        val updatedSum = sum + calc
        innerFun(x,index+1,((updatedSum * 10000).round / 10000.toDouble))
      }
    }
    innerFun(x,0,0)
  }
  def factorial(n: Int): Int =
  {
    if (n == 0) 1
    else n * factorial(n-1)
  }
  println(eulerNumberCalc(20))
}
