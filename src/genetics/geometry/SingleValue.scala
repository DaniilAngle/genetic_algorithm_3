package genetics.geometry

object SingleValue {

  def costFunction(number: Double): SingleValue => Double = {
    SingleValue => SingleValue.value - number
  }

  def incubator(genes: List[Double]): SingleValue = {
    new SingleValue(genes.head)
  }

}

class SingleValue(var value: Double){}
