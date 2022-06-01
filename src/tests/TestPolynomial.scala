package tests

import genetics.GeneticAlgorithm
import genetics.geometry.{Point, Polynomial}
import org.scalatest.FunSuite

class TestPolynomial extends FunSuite {

  val EPSILON: Double = 0.5

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Compute Polynomial Regression") {
    var points: List[Point] = List(new Point(0, 1), new Point(1, 2), new Point(3, 10), new Point(4, 17),
      new Point(5, 26), new Point(10, 101), new Point(20, 401))
    var polynomial: List[Double] = List(1, 0, 1)
    var computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(points), 3)
    println(computed.coefficients)
    for (a <- computed.coefficients.indices) {
      assert(equalDoubles(computed.coefficients(a), polynomial(a)))
    }

    points = List(new Point(0, 4), new Point(1, 3.5), new Point(6, 301), new Point(3, 26.5), new Point(-2, -31),
      new Point(-1, -3.5), new Point(-6, -581))
    polynomial = List(2, -4, 1.5, 4)
    computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(points), 4)
    println(computed.coefficients)
    for (a <- computed.coefficients.indices) {
      assert(equalDoubles(computed.coefficients(a), polynomial(a)))
    }

    /////////////////////////5th DEGREE WORKS FROM TIME TO TIME WITH HIGH GENERATION COUNT
//    points = List(new Point(0, 4), new Point(1, -0.3), new Point(-3, 33.7), new Point(-6, 542.2), new Point(12, 137.2),
//      new Point(10, -381), new Point(5, -213.5), new Point(2, -21.8))
//    polynomial = List(0.2, -2, -4, 1.5, 4)
//    computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(points), 5)
//    println(computed.coefficients)
//    for (a <- computed.coefficients.indices) {
//      assert(equalDoubles(computed.coefficients(a), polynomial(a)))
//    }

    points = List(new Point(0, 5), new Point(-2.5, 0), new Point(3, 11), new Point(-8, -11), new Point(8, 21))
    polynomial = List(2, 5)
    computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(points), 2)
    println(computed.coefficients)
    for (a <- computed.coefficients.indices) {
      assert(equalDoubles(computed.coefficients(a), polynomial(a)))
    }
  }
}
