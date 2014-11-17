import cmpsci220.hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = {
    var board: Map[(Int, Int), Set[Int]] = Map()
    var peersLst = peers(0, 0)

    def parseRecurse(str: String, row: Int, col: Int): Unit = {
      var lst = Set(1,2,3,4,5,6,7,8,9)
      var kv: ((Int, Int), Set[Int]) = ((row,col), lst)
      
      if(str.head != '.') {
        kv = ((row, col), Set(str.head.asDigit))
      }
      else if(str.head == '.') {
        kv = ((row, col), Set(1,2,3,4,5,6,7,8,9))
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


    // val someOfOne = """Some\(/d\)""".r

    // for((k,v) <- board) {
      
    //   var lstTmp: List[Int] = List()
      
    //   if(v == List(1,2,3,4,5,6,7,8,9)) {
    //     k match {
    //       case (a,b) => peersLst = peers(a,b) 
    //     }
    //     for(peer <- peersLst) {
    //       board.get(k).toString match {
    //         case someOfOne(_*) => lstTmp = v diff board.get(k).get
    //         case _ => Unit
    //       } 
    //     }

    //   board = board + ((k, lstTmp))
    //   }
    
    // }
    new Board(board)
  }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): Set[(Int, Int)] = {
    var tmp: Set[(Int, Int)] = Set()
    for(i <- 0 to 8) {
      if(i != col) {
        tmp = tmp ++ Set((row, i))
      }
      if(i != row)
        tmp = tmp ++ Set((i, col))
    }
    tmp ++ box(row, col)
  }

  def box(row: Int, col: Int): Set[(Int, Int)] = {
    var tmp: Set[(Int, Int)] = Set()
    var box: (Int, Int) = (row/3, col/3)
    for (i <- 0 to 8;j <- 0 to 8) {
      if((i/3,j/3) == (row/3, col/3) && (i,j) != (row, col)) {
        tmp = tmp ++ Set((i,j))
      }
    }
    tmp
  }
}

// Top-left corner is (0,0). Bottom-right corner is (8,8).
// You don't have to have a field called available. Feel free to change it.
class Board(val available: Map[(Int, Int), Set[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): Set[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toSet)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    available.get((row, col)) match {
      case x : Set() => 
    }
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

  override def toString = {
    available.toString
  }
}