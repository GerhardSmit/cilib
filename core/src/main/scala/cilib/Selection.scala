package cilib

object Selection {

  def indexNeighbours[A](n: Int): Selection[A] =
    (l: List[A], x: A) => {
      val size = l.size
      val y = n / 2
      val point = (l.indexOf(x) - (y) + size) % size

      Stream.continually(l).flatten.drop(point).take(n).toList
    }

  def gbest[A]: Selection[A] =
  (l: List[A], x: A) => {
  	if(l.contains(x))
  		l
  	else
  		Nil  		
  }

  //def selfless
    
}
