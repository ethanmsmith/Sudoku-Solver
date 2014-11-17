import Solution._
import cmpsci220.hw.sudoku._

class TestSuite extends org.scalatest.FunSuite {
	test("parse") {
		println(parse("..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."))
	}

	test("full board isSolved") {
		assert(parse("233383333363373384333533239333135438333333333432736333331337343723343363334313333").isSolved)
	}

	test("boar isUnsolvable") {
		assert(parse("233.83333363373384333533239333135438333333333432736333331337343723343363334313333").isUnsolvable)
	}
}