import Solution._
import cmpsci220.hw.sudoku._

class TestSuite extends org.scalatest.FunSuite {
	test("peers testing"){
		println("peers 0,0")
		println(peers(0,0))
		println("peers 2,2")
		println(peers(2,2))
		println("peers 4,4")
		println(peers(4,4))
		println("peers 6,6")
		println(peers(6,6))
		println("peers 8,8")
		println(peers(8,8))
		println()
	}
	test("parse") {
		println(parse("..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.."))
	}
	test("parse2") {
		println(parse("2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"))
	}
}