package genetics.geometry


object Polynomial {

  def costFunction(points: List[Point]): Polynomial => Double = {
    Polynomial => var eval: Double = 0.0
    for (point <- points) {
      eval += Math.abs(Polynomial.evaluate(point.x) - point.y)
    }
    eval
  }

  def incubator(genes: List[Double]): Polynomial = {
    new Polynomial(genes)
  }

}

class Polynomial(var coefficients: List[Double]) {

  def evaluate(x: Double): Double = {
    var result: Double = 0.0
    var power: Int = coefficients.size - 1
    for (coefficient <- coefficients) {
      if (power != 0) {
        result += coefficient * Math.pow(x, power)
        power -= 1
      } else {
        result += coefficient
      }
    }
    result
  }

}
