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
            case Some(x) => x match { 
              case y::Nil => lstTmp -= y
              case y::z => lstTmp
              case _ => Unit
            }
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
      case Nil => Some(99)
      case x::Nil => Some(x)
      case x::y => None
    }
  }

  def isSolved(): Boolean = {
    for ((k,v) <- available) {
      v match {
        case Nil => return false
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
      var peerList = k match {case (a,b) => Solution.peers(a,b)}
      for(peer <- peerList) {
        var kRow = 0
        var kCol = 0
        k match {case (a,b) => kRow = a; kCol = b}
        if(valueAt(kRow, kCol) != None && available.get(peer) != None) {
          if(v == available.get(peer).get) {
            return true
          }
        }
      }
    }
    return false
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    var board = available
    var kv = ((row, col), List(value))
    board += (kv)
    val peerList = Solution.peers(row,col)
    for(peer <- peerList) {
      var pSet: Set[Int] = board.get(peer).get.toSet
      pSet -= value
      val pKv = (peer, pSet.toList)
      board += pKv
      available.get(peer).get match {
        case Nil => Unit
        case x::Nil => removeFromPeers(peer, x)
        case x::y => Unit
      }
    }

    /*def removeFromPeers(peer: (Int, Int), value: Int) = {
      val row = peer match{case (a,b) => a}
      val col = peer match{case (a,b) => b}
      for(key <- Solution.peers(row, col)) {
        val pRow = key match{case (a,b) => a}
        val pCol = key match{case (a,b) => b}
        var peerList: Set[Int] = available.get(key).get.toSet
        peerList -= value
        var rmKv = (key,peerList.toList)
        board += rmKv
      }
    }*/

    new Board(board)
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

  def toString2(): String = {
    var str: String = ""
    for(i <- 0 to 8){
      if(i == 0 || i == 3 || i == 6) str = str + "-------------\n"
        for(j <- 0 to 8){
          if(j == 0 || j == 3 || j == 6) str = str + "|"
          val ls:List[Int] = available((i, j))
          if(ls.size == 1) str = str + ls(0)
          else str = str + "."
          if(j == 8) str = str + "|"
        }
        str = str + "\n"
        if(i == 8) str = str + "-------------\n"
      }
      return str
  }
}