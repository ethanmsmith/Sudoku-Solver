import Solution._
import cmpsci220.hw.sudoku._

//GradedTestSuite modified to actually compile, then work :)
class GradedTestSuite extends org.scalatest.FunSuite {
	var kv: ((Int, Int), List[Int]) = ((3,5),List(3))
	val board1: Map[(Int, Int), List[Int]] = Map() + kv
	val board2: Board = Solution.parse("..3456789..4........5........6........7........8........9........1........2......")

	test("valueAt works for a very basic board") {
		val tmp = new Board(board1)
		assert(tmp.valueAt(3,5) == Some(3))
	}

	test("parse works for sample string assuming the above valueAt works") {
		assert(Solution.parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").valueAt(0,4) == Some(8))
	}

	test("peers works for a simple board") {
		assert(peers(0,2).toSet == Set((1,0),(1,1),(2,1),(2,0),(0,0),(0,1),(0,3),(0,4),(0,5),(0,6),
										(0,7),(0,8),(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)))
	}

	test("valueAt works for a non-valid board2") {
		kv = ((6,8),List(3))
		val tmp = Map() + kv
		assert(new Board(tmp).valueAt(6,8) == Some(3))
	}

}