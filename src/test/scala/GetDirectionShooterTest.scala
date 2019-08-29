import org.scalatest.{FlatSpec}

class GetDirectionShooterTest extends FlatSpec {
  "make shot" should "work" in {
    val enemyBoardView = Array(Array(FieldState.SunkShip, FieldState.MissShot),
      Array(FieldState.SunkShip, FieldState.Empty))
    val coordinates = GetDirectionShooter(0, 1).makeShot(enemyBoardView)
    assert(coordinates == (1, 1))
  }
}
