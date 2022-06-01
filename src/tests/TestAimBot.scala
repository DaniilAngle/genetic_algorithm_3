package tests

import genetics.GeneticAlgorithm
import genetics.aimbot.{AimBot, PhysicsVector}
import org.scalatest.FunSuite

class TestAimBot extends FunSuite {

  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithms Hits a Moving target") {
    val targetLocation: PhysicsVector = new PhysicsVector(3,0)
    val sourceLocation: PhysicsVector = new PhysicsVector(5, 0)
    val targetVelocity: PhysicsVector = new PhysicsVector(5,10)
    var computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLocation, targetLocation, targetVelocity), 2)
    println(computed.toString)
  }

}
