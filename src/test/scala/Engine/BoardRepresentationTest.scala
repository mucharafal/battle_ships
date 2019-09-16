package Engine


import Engine.FieldState._

import org.scalatest.FunSuite

class BoardRepresentationTest extends FunSuite {

  test("testGetViewForEnemy") {
    val field1 = Field(Point(10, 10), SunkShip)
    val field2 = Field(Point(9, 9), AliveShip)
    val boardRepresentation = BoardRepresentation(List(field1, field2))
    val boardRepresentationForEnemy = BoardRepresentation(List(field1))
    assert(boardRepresentation.getViewForEnemy == boardRepresentationForEnemy)
  }


  test("testGetStateOf") {
    val emptyBoard = BoardRepresentation(List())
    assert(emptyBoard.getStateOf(Point(-1, 100)) == Empty)

    val field1 = Field(Point(10, 10), SunkShip)
    val board = BoardRepresentation(List(field1))
    assert(board.getStateOf(Point(10, 10)) == SunkShip)
  }
}
