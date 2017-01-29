package adventofcode.day2

object BathroomSecurity {
	def getInputLines = scala.io.Source.fromFile("input.txt").getLines

	case class Pos(x: Int, y: Int)

	def processInstruction(lastMove: Pos, nextMove: Char): Pos = {
		nextMove match {
			case 'U' => Pos(lastMove.x, lastMove.y - 1)
			case 'D' => Pos(lastMove.x, lastMove.y + 1)
			case 'L' => Pos(lastMove.x - 1, lastMove.y)
			case 'R' => Pos(lastMove.x + 1, lastMove.y)
		}
	}

	def getNextMove(lastMove: Pos, nextMove: Char): Pos = {
		val goodPositions = (0 to 2).flatMap(x => (0 to 2).map(y => Pos(x,y)))
		val nextRawPos = processInstruction(lastMove, nextMove)
		if (goodPositions.contains(nextRawPos)) nextRawPos else lastMove
	}

	def posToDigit(pos: Pos) = {
		(pos.x+1 + pos.y*3).toString.head
	}

	def processLine(startPos: Pos, toChar: Function1[Pos,Char], action: Function2[Pos,Char,Pos])(line: String) : Char = {
		toChar(line.foldLeft(startPos)(action))
	}

	def task1 = {
		getInputLines.map(processLine(Pos(1,1), posToDigit, getNextMove))
			.mkString
	}

	def getNextMove2(lastMove: Pos, nextMove: Char): Pos = {
		val goodPositions = Set(Pos(2,0), Pos(1,1), Pos(2,1), Pos(3,1), Pos(0,2), Pos(1,2), Pos(2,2), Pos(3,2), Pos(4,2), Pos(1,3), Pos(2,3), Pos(3,3), Pos(2,4))
		val nextRawPos = processInstruction(lastMove, nextMove)
		if (goodPositions.contains(nextRawPos)) nextRawPos else lastMove
	}

	def posToDigit2(pos: Pos): Char = {
		pos match {
			case Pos(2,0) => '1'
			case Pos(1,1) => '2'
			case Pos(2,1) => '3'
			case Pos(3,4) => '4'
			case Pos(0,2) => '5'
			case Pos(1,2) => '6'
			case Pos(2,2) => '7'
			case Pos(3,2) => '8'
			case Pos(4,2) => '9'
			case Pos(1,3) => 'A'
			case Pos(2,3) => 'B'
			case Pos(3,3) => 'C'
			case Pos(2,4) => 'D'
			case _ => 'Z'
		}
	}

	def task2 = {
		getInputLines.map(processLine(Pos(0,2), posToDigit2, getNextMove2))
			.mkString
	}

	def main(args: Array[String]): Unit = {
		println("task 1 result:")
		println(task1)
		println("task 2 result:")
		println(task2)
	}
}