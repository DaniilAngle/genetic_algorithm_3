package tests

import genetics.GeneticAlgorithm
import genetics.geometry.SingleValue
import org.scalatest.FunSuite

import java.util.random.RandomGenerator
import scala.util.Random

class TestSingleValue extends FunSuite {

  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Finds a Random Number") {
    var hiddenNumber: Double = 50000
    var computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))

    hiddenNumber = 50.5
    computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))

    hiddenNumber = 0.0
    computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))

    hiddenNumber = -10.05
    computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))

    hiddenNumber = -1000.35
    computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }

}
