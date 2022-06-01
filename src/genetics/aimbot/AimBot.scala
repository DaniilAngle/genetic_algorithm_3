package genetics.aimbot

object AimBot{

  var numberOfDimensions: Int = 0

  val projectileSpeed: Double = 7.0


  def costFunction(sourceLocation: PhysicsVector, targetLocation: PhysicsVector, targetVelocity: PhysicsVector): PhysicsVector => Double = {
    PhysicsVector => val guess: PhysicsVector = PhysicsVector
      val guessedMovement: PhysicsVector = guess.normal2d()
      targetVelocity.normal2d()
    val slopeOfTarget: Double = targetVelocity.y / targetVelocity.x
    val bTarget: Double = targetLocation.y - slopeOfTarget * targetLocation.x
    val slopeOfProjectile: Double = guessedMovement.y / guessedMovement.x
    val bProjectile: Double = sourceLocation.y - slopeOfProjectile * sourceLocation.x
    val xIntersection: Double = (bProjectile - bTarget) / (slopeOfTarget - slopeOfProjectile)
    val sOfProjectile: Double = (xIntersection - sourceLocation.x) / guessedMovement.x
    val intersectionPoint: PhysicsVector = new PhysicsVector(sourceLocation.x + sOfProjectile * guessedMovement.x,
      sourceLocation.y + sOfProjectile * guessedMovement.y)
    val targetMovementSpeed: Double = Math.sqrt(Math.pow(targetVelocity.x, 2.0) + Math.pow(targetVelocity.y, 2.0))
    val tMovementRatio: Double = targetMovementSpeed / (projectileSpeed * sOfProjectile)
    val positionAtTime: PhysicsVector = new PhysicsVector(targetLocation.x + tMovementRatio * targetVelocity.x,
      targetLocation.y + tMovementRatio * targetVelocity.y)
      if (sOfProjectile < 0) {
        100000.0
      } else {
        intersectionPoint.distance2d(positionAtTime) + (guess.x.abs - guessedMovement.x.abs * projectileSpeed + guess.y.abs -
          guessedMovement.y.abs * projectileSpeed + guess.z.abs - guessedMovement.z.abs * projectileSpeed).abs
      }
  }


  def incubator(genes: List[Double]): PhysicsVector = {
    numberOfDimensions = genes.size
    if (numberOfDimensions == 3) {
      new PhysicsVector(x = genes.head, y = genes(1), z = genes.last)
    } else {
      new PhysicsVector(x = genes.head, y = genes.last)
    }
  }
}
