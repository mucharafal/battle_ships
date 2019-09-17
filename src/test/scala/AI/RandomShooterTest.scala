package AI

import Engine._
import org.scalatest.FlatSpec

class RandomShooterTest extends FlatSpec {

  "make shot" should "return coordinates of available place" in {
    val fields = for(point <- Point(0, 0) until Point(Board.getSize, Board.getSize)) yield Field(point, FieldState.MissShot)
    val selectedPoint = Point(7, 6)
    val boardRepresentation = BoardRepresentation(fields.toList.filter(x => x.point != selectedPoint))
    val coordinates = RandomShooter().makeShot(boardRepresentation)
    assert(coordinates == selectedPoint)
  }
}
