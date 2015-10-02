package section02

/**
 * Created by Daesap on 2015-10-02.
 */
object Practice {

	// Practice 2-1.
	def fibonacci(n: Int): Int = {
		def go(n: Int, acc: Int): Int =
			if (n <= 0) acc
			else go(n - 1, n + acc)

		go(n, 0)
	}

	// Practice 2-2.
}
