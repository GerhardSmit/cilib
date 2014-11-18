package cilib

import scala.math

object Selection {

  import Distance._

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

  def find[A](l: List[A],n: Int, r: Int, c: Int) : A =
  	l(r * n + c) 

  def vonNeumannNeighbourhood[A] : Selection[A] =
  (l: List[A], x: A) => {
  	val np = l.size
  	val index = l.indexOf(x)
  	val sqSide = scala.math.round(scala.math.sqrt(np)).toInt
  	val nRows = scala.math.ceil(np/sqSide.toDouble).toInt
  	val row = (index / sqSide).toInt
  	val col = (index % sqSide).toInt

  	def f(i: Int) : Int = if(i == nRows - 1) np - i * sqSide else sqSide

  	val north = find(l, sqSide, (row - 1 + nRows) % nRows - (if(col >= f((row - 1 + nRows) % nRows)) 1 else 0), col)
  	val south = find(l, sqSide, (if(col >= f((row + 1) % nRows)) 0 else (row + 1 ) % nRows), col)
  	val east = find(l, sqSide, row, (col + 1) % f(row))
  	val west = find(l, sqSide, row, (col - 1 + f(row)) % f(row))
  	List(x, north, east, south, west)
  }

  def hypercubeNeighbourhood[A](n: Int) : Selection[A] =
  (l: List[A], x: A) => {
  	val index = l.indexOf(x)
  	val list = (0 to (n - 1)).toList.map((x:Int) => (index ^ scala.math.pow(2 , x).toInt).toInt)
  	l.zipWithIndex.filter(x => list.exists(y => y == x._2)).unzip._1

  }

  def distanceBased[S](n: Int,d: Distance)(implicit ord: math.Ordering[Double]) : Selection[(S,Pos[Double])] = 
  (l: List[(S,Pos[Double])], x: Pos[Double]) => {
  	val position = x.pos
  	val distances = l.map{case (x,y) => y.pos}

  }
    
}
