package Engine

case class Point(x: Int, y: Int) {
  def right(shift: Int): Point = {
    Point(x, y + shift)
  }

  def right: Point = Point(x, y + 1)

  def left(shift: Int): Point = {
    Point(x, y - shift)
  }

  def left: Point = Point(x, y - 1)

  def down(shift: Int): Point = {
    Point(x + shift, y)
  }

  def down: Point = Point(x + 1, y)

  def up(shift: Int): Point = {
    Point(x - shift, y)
  }

  def up: Point = Point(x - 1, y)

  def isOnHorizontalLineBetween(begin: Point, end: Point): Boolean = {
    (begin.y to end.y).exists(newY => Point(begin.x, newY) == this)
  }

  def isOnVerticalLineBetween(begin: Point, end: Point): Boolean = {
    (begin.x to end.x).exists(newX => Point(newX, begin.y) == this)
  }

  def to(otherPoint: Point): IndexedSeq[Point] = {
    for (i <- x to otherPoint.x;
         j <- y to otherPoint.y)
      yield Point(i, j)
  }

  def until(otherPoint: Point): IndexedSeq[Point] = {
    for(i <- x until otherPoint.x;
        j <- y until otherPoint.y)
      yield Point(i, j)
  }
}
