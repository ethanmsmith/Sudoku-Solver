import cmpsci220.hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = {
    throw new UnsupportedOperationException("not implemented")
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    var tmp: List[(Int, Int)] = List()
    for(i <- 0 to 8) {
      if(i != col) {
        tmp = tmp :+ (row, i)
      }
      if(i != row)
        tmp = tmp :+ (i, col)
    }
    //GET VALUES IN THE SAME BLOCK
    tmp
  }
}

// Top-left corner is (0,0). Bottom-right corner is (8,8).
// You don't have to have a field called available. Feel free to change it.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    throw new UnsupportedOperationException("not implemented")
  }

  def isSolved(): Boolean = {
    throw new UnsupportedOperationException("not implemented")
  }

  def isUnsolvable(): Boolean = {
    throw new UnsupportedOperationException("not implemented")
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    throw new UnsupportedOperationException("not implemented")
  }

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    if (isUnsolvable()) {
      return List()
    }

    throw new UnsupportedOperationException("not implemented")
  }

  def solve(): Option[Board] = {
    throw new UnsupportedOperationException("not implemented")
  }
}