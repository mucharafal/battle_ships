import AI.FindDirectionShooter
import Engine.FieldState
import org.scalatest.FlatSpec

class FindDirectionShooterTest extends FlatSpec {
  "make shot" should "work" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.MissShot),
      Array(FieldState.SunkShip, FieldState.Empty))
    val coordinates = FindDirectionShooter(0, 1).makeShot(enemyBoardView)
    assert(coordinates == (1, 1))
  }
}
