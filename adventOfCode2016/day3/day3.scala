package adventofcode.day3

object SquaresWithThreeSides extends App {
    println(
    	scala.io.Source.fromFile("input.txt")
    	.getLines.map(line => line.split(' ')
    		.map(_.trim)
    		.filter(_.isEmpty == false)
    		.map(_.toInt))
    	.map(a => (a(0),a(1),a(2)))
		.filter(t => (t._1 + t._2 > t._3) && (t._1 + t._3 > t._2) && (t._2 + t._3 > t._1))
    	.size
		)
}
