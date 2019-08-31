import AI.RandomShooter
import Engine.FieldState
import org.scalatest.{FlatSpec}

class RandomShooterTest extends FlatSpec {

  "make shot" should "return coordinates ne of avaliable place" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.MissShot),
      Array(FieldState.SunkShip, FieldState.Empty))
    val coordinates = RandomShooter().makeShot(enemyBoardView)
    assert(coordinates == (1, 1))
  }
}
