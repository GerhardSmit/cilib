package cilib

import spire._
import spire.implicits._

object Distance {
	def minkowski(p: Double) =
		(x: List[Double], y: List[Double]) => x.zip(y).map { case (a, b) =>
		math.abs(a - b) ** p }.sum ** (1 / p)

	val manhattan = minkowski(1)
	val euclidean = minkowski(2)
}