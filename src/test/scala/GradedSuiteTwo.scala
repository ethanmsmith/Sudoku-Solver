import Solution._
import cmpsci220.hw.sudoku._

//tests made to actually compile and work :)
//names changes slightly to make testing easier
class GradedTestSuiteTwo extends org.scalatest.FunSuite {

	val boardSolved = Solution.parse("483967251921345876657821493548729136132564798976138245372814695689253417514769382")
	val boardUnsolved = Solution.parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.")

	test("board is solvable") {
		assert(parse(".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.").isUnsolvable == false)
	}

	test("board with duplicate values isUnsolvable") {
		assert(parse("..3456789..4........5........7........7........8........9........1........2......").isUnsolvable)
	}

	test("isSolved returns true for legitimately solved board") {
		assert(boardSolved.isSolved)
	}

	test("isSolved returns false for incomplete board") {
		assert(boardUnsolved.isSolved == false)
	}
}