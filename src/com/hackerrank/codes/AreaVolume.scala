package com.hackerrank.codes

import scala.annotation.tailrec

/**
 * @author sahilgogna on 2020-03-09
 */
object AreaVolume extends App {

  def f(coefficients:List[Int],powers:List[Int],x:Double):Double = {
    val length = powers.length
    def tempFunc(index:Int,acc:Double):Double = {
      if(index<0) acc
      else tempFunc(index-1, acc = acc + (coefficients(index) * scala.math.pow(x,powers(index))))
    }
    tempFunc(length-1,0)
  }

  def area(coefficients:List[Int],powers:List[Int],x:Double):Double = {
    math.Pi * math.pow(f(coefficients,powers,x),2)
  }
  def summation(func:(List[Int],List[Int],Double)=>Double,upperLimit:Int,lowerLimit:Int,coefficients:List[Int],powers:List[Int]):Double =
  {
    val step: Double = 0.001
    val y: Seq[Double] = for {
      x <- 1 to 4
    } yield 1 + x * 0.001
    y.map( a => func(coefficients,powers,a)*step).sum
  }


}
