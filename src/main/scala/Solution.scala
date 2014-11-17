import cmpsci220.hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = {
    var board: Map[(Int, Int), List[Int]] = Map()
    

    def parseRecurse(str: String, row: Int, col: Int): Unit = {
      var kv: ((Int, Int), List[Int]) = ((row,col), List())
      
      if(str.head != '.') {
        kv = ((row, col), List(str.head.asDigit))
      }
      else if(str.head == '.') {
        kv = ((row, col), List(0))
      }

      board = board + kv
      if(col == 8 && row != 8) {
        parseRecurse(str.substring(1), row+1, 0)
      }
      else if(col != 8 && row !=8) {
        parseRecurse(str.substring(1), row, col+1)
      }
    }
    parseRecurse(str, 0, 0)
    for((k,v) <- board) {
      var peersLst: List[(Int, Int)] = List()
      var lstTmp: Set[Int] = Set(1,2,3,4,5,6,7,8,9)
      
      if(v == List(0)) {
        k match {
          case (a,b) => peersLst = peers(a,b)
          //println("peers of" + k + peersLst)
        }
        for(peer <- peersLst) {
          board.get(peer) match {
            case None => lstTmp
            case Some(x) => x match { case y::Nil => lstTmp -= y
                                      case y::z => lstTmp}
            case _ => Unit
          } 
        }

      board = board + ((k, lstTmp.toList))
      }
    
    }
    new Board(board)
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = {
    var tmp: List[(Int, Int)] = List()
    for(i <- 0 to 8) {
      if(i != col) {
        tmp = tmp ++ List((row, i))
      }
      if(i != row)
        tmp = tmp ++ List((i, col))
    }
    tmp ++ box(row, col)
  }

  def box(row: Int, col: Int): List[(Int, Int)] = {
    var tmp: List[(Int, Int)] = List()
    var box: (Int, Int) = (row/3, col/3)
    for (i <- 0 to 8;j <- 0 to 8) {
      if((i/3,j/3) == (row/3, col/3) && (i,j) != (row, col)) {
        tmp = tmp ++ List((i,j))
      }
    }
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
    available.get((row, col)).get match {
      case x::Nil => Some(x)
      case x::y => None
      case _ => None
    }
  }

  def isSolved(): Boolean = {
    for ((k,v) <- available) {
      v match {
        case x::Nil => Unit
        case x::y => return false
      }
    }
    return true
  }

  def isUnsolvable(): Boolean = {
    for((k,v) <- available) {
      v match {
        case Nil => return true
        case x::y => Unit
      }
    }
    return false
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

  override def toString = {
    available.toString
  }
}