package adventofcode.day1
import scala.util.matching.Regex

object TaxiCab {
	case class Step(size: Int, rotation: Seq[Int])
	case class Vec(x: Int, y: Int, factor: Int)

	def convertToVector(dir: Vec, step: Step) =
		Vec(dir.x * step.rotation(0) + dir.y * step.rotation(1),
			dir.x * step.rotation(2) + dir.y * step.rotation(3),
			step.size)

	def walk(a: Vec, b: Vec) = Vec(a.x * a.factor + b.x * b.factor, a.y * a.factor + b.y * b.factor, 1)

	def getParsedInput = {
		val path = scala.io.Source.fromFile("input.txt").mkString
		val leftStep = """L(\d+)""".r
		val rightStep = """R(\d+)""".r

		path.split(", ").map({
			case leftStep(length) => Step(length.toInt, Seq(0,-1,1,0))
			case rightStep(length) => Step(length.toInt, Seq(0,1,-1,0))
		})
	}

	def addVectorsInBetween(a: Seq[Vec], b: Vec) : Seq[Vec] = {
		if (a.size != 0 && !(a.last.x == b.x && a.last.y == b.y)) {
			(for {
				x <- a.last.x to b.x by (if (a.last.x > b.x) -1 else 1)
				y <- a.last.y to b.y by (if (a.last.y > b.y) -1 else 1)
			} yield Vec(x, y, 0)).drop(1)
		}
		else Seq(b)
	}

	case class SetAndVector(set: Set[Vec], v: Vec, seen: Boolean)

	def seen(a: SetAndVector, b: Vec) : SetAndVector = {
		SetAndVector(a.set + b, b, a.set.contains(b))
	}

	def task1 = {
		getParsedInput
			.scanLeft(Vec(0,1,0))(convertToVector)
			.reduceLeft(walk)
	}

	def task2 = {
		getParsedInput
			.scanLeft(Vec(0,1,0))(convertToVector)
			.scanLeft(Vec(0,0,0))(walk)
			.scanLeft(Seq[Vec]())(addVectorsInBetween)
			.flatten
			.scanLeft(SetAndVector(Set(), Vec(0,0,0), false))(seen)
			.filter(_.seen)
			.head
			.v
	}

	def main(args: Array[String]): Unit = {
		println("task1 result:")
		println(task1)
		println("task2 result:")
		println(task2)
	}
}