package cilib

import scala.math

object Selection {

  def indexNeighbours[A](n: Int): Selection[A] =
    (l: List[A], x: A) => {
      val size = l.size
      val y = n / 2
      val point = (l.indexOf(x) - (y) + size) % size

      Stream.continually(l).flatten.drop(point).take(n).toList
    }

  def gbest[A]: Selection[A] =
  (l: List[A], A) => {
  		l		
  }

  def selflessBest[A](n: Int): Selection[A] = 
  (l: List[A], x: A) => {
  	val lbest = indexNeighbours(n)(l,x)
  	lbest.filter(_ != x)
  }

  def find[A](l: List[A],n: Int, r: Int, c: Int) : Int =
  	l.indexOf(r * n + c) 

  def f(r: Int,nRows: Int, np : Int, sqSide : Int) : Int =
 	if(r == nRows - 1)
 		np - r * sqSide
 	else
 		sqSide

  def vonNeumannNeighbourhood[A] : Selection[A] =
  (l: List[A], x: A) => {
  	val np = l.size
  	val index = l.indexOf(x)
  	val sqSide = scala.math.round(scala.math.sqrt(np)).toInt
  	val nRows = scala.math.ceil(np/sqSide.toDouble).toInt
  	val row = (index / sqSide).toInt
  	val col = (index % sqSide).toInt
  }
    
}
