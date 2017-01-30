package adventofcode.day3

object SquaresWithThreeSides extends App {
	val lines = scala.io.Source.fromFile("input.txt").getLines.toList
	println(
		lines
			.map(line => line.split("""\s+""")
				.filter(!_.isEmpty)
				.map(_.toInt))
			.count(a => (a(0) + a(1) > a(2)) && (a(0) + a(2) > a(1)) && (a(1) + a(2) > a(0)))
	)

	val arr = lines
			.map(line => line.split("""\s+""")
			.map(_.trim)
			.filter(!_.isEmpty)
			.map(_.toInt))
		.toArray

	val triplets = for {
		a <- 0 to arr.length - 3 by 3
		b <- 0 to 2
		} yield Seq(arr(a)(b), arr(a+1)(b), arr(a+2)(b))

	val res = triplets.count(a => (a(0) + a(1) > a(2)) && (a(0) + a(2) > a(1)) && (a(1) + a(2) > a(0)))
	println(res)
}
