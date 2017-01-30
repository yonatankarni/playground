package adventofcode.day3

object SquaresWithThreeSides extends App {
	println(
		scala.io.Source.fromFile("input.txt").getLines
			.map(line => line.split(' ')
				.map(_.trim)
				.filter(!_.isEmpty)
				.map(_.toInt))
			.map(a => (a(0),a(1),a(2)))
			.filter(t => (t._1 + t._2 > t._3) && (t._1 + t._3 > t._2) && (t._2 + t._3 > t._1))
			.size
	)

	val arr = scala.io.Source.fromFile("input.txt").getLines
		.map(line => line.split(' ')
			.map(_.trim)
			.filter(!_.isEmpty)
			.map(_.toInt))
		.toArray

	val triplets = for {
		a <- 0 to arr.length - 3 by 3
		b <- 0 to 2
		} yield Seq(arr(a)(b), arr(a+1)(b), arr(a+2)(b))

	val res = triplets.map(a => (a(0),a(1),a(2)))
		.filter(t => (t._1 + t._2 > t._3) && (t._1 + t._3 > t._2) && (t._2 + t._3 > t._1))
		.size
	println(res)
}
