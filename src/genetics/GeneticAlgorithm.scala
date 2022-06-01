package genetics

import scala.collection.immutable.ListMap
import scala.util.Random


object GeneticAlgorithm {

  /**
    * @param incubator Determines how instances of type T are created from a List of Doubles (genes)
    * @param costFunction Determines the cost for a given instance of T
    * @param numberOfGenes The size of the List expected by the incubator
    * @tparam T The type to be optimized
    * @return An instance of T with minimal cost
    */
  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    var ANIMAL_PER_GENERATION: Int = 100
    var GENERATION: Int = 100000
    var BEST_ANIMALS: Int = 15
    var MUTATION_FREQUENCY: Double = 0.6

    var animalList: List[List[Double]] = List.empty
    var costList: List[Double] = List.empty

    if (numberOfGenes >= 5) {
      GENERATION = 300000
    }

    def mutate(animal: List[Double]): List[Double] = {
      var mutatedAnimalGenes: List[Double] = List.empty
      for (element <- animal) {
        var gene: Double = element
        if (animal.size > 1)  {
          if (Random.nextDouble() <= MUTATION_FREQUENCY) {
            gene = gene * (Math.random() * 0.25 + 0.93)
            if (Random.nextDouble() <= 0.2) {
              if (Random.nextDouble() <= 0.5) {
                gene -= 2
              } else {
                gene += 2
              }
            }
          }
        } else {
          if (gene > -0.0001 && gene < 0.0001) {
            gene = -2
          }
          gene *= (Math.random() * 0.2 + 0.9)
        }
        mutatedAnimalGenes ::= gene
      }
      mutatedAnimalGenes
    }

    def crossbreed(animal1: List[Double], animal2: List[Double]): List[Double] ={
      var bredAnimalGenes: List[Double] = List.empty
      for (a <- animal1.indices) {
        var gene: Double = animal1(a)
        if (animal1.size == 1) {
          gene = (animal1(a) + animal2(a))/2
        } else {
          if (Random.nextDouble() <= MUTATION_FREQUENCY) {
            gene = animal2(a)
//            if ((animal1a).signum == 1 && animal2(a).signum == 1) {
//              gene = (animal1(a).abs + animal2(a).abs)/2
//            } else {
//              gene = (animal1(a).abs + animal2(a).abs)/2 * (-1)
//            }
          }
        }
        bredAnimalGenes ::= gene
      }
      bredAnimalGenes
    }

    for (animal <- 1 to ANIMAL_PER_GENERATION) {
      var animalGene: List[Double] = List.empty
      for (a <- 1 to numberOfGenes) {
        if (numberOfGenes > 1) {
          if(Random.nextDouble() >= 0.5) {
            animalGene ::= Random.nextInt(50000).toDouble / 2.3
          } else {
            animalGene ::= (Random.nextInt(50000).toDouble / 2.3) * (-1)
          }
        } else {
          animalGene ::= Random.nextInt(50000).toDouble / 2.3
        }
      }
      animalList ::= animalGene
    }
    def sortByCost(): List[List[Double]] = {
      var generationCostMap: Map[List[Double], Double] = Map.empty
      for (animalGene <- animalList) {
        generationCostMap += animalGene -> costFunction(incubator(animalGene))
      }
      generationCostMap = ListMap(generationCostMap.toSeq.sortWith(_._2.abs < _._2.abs):_*)
      val mapToList: List[List[Double]] = generationCostMap.keys.toList
      mapToList
    }

    for (a <- 1 to GENERATION) {
      costList = List.empty
      val mapToList: List[List[Double]] = sortByCost()
      animalList = List.empty
      for (a <- 0 until BEST_ANIMALS) {
        animalList ::= mapToList(a)
      }
      if (costFunction(incubator(animalList.last)) <= 0.001 && costFunction(incubator(animalList.last)) >= -0.001) {
        return incubator(animalList.head)
      }
      while (animalList.size != ANIMAL_PER_GENERATION) {

        // crossbreeding
        val firstAnimal: Int = Random.nextInt(BEST_ANIMALS)
        var secondAnimal: Int = Random.nextInt(BEST_ANIMALS)
        while (firstAnimal == secondAnimal) {
          secondAnimal = Random.nextInt(BEST_ANIMALS)
        }
        animalList ::= crossbreed(animalList(firstAnimal), animalList(secondAnimal))

        // mutations
        if (animalList.size != ANIMAL_PER_GENERATION) {
          val animal: List[Double] = animalList(Random.nextInt(BEST_ANIMALS - 1))
          animalList ::= mutate(animal)
        }
      }
    }
    println("Cost: ", costFunction(incubator(sortByCost().head)))
//    println("list:", sortByCost())
    incubator(sortByCost().head)
  }
}
