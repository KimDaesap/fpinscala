import section02._

// example.
Example.factorial(1)
Example.factorial(10)

// test 2-1.
Practice.fibonacci(1)
Practice.fibonacci(10)

// test 2-2.
Practice.isSorted(Array(1,2,3,4,5), (a: Int, b: Int ) => a < b)

// test 2-3.
Practice.curry((a: Int, b: Int) => a+b)(1)(2)

// test 2-4.
Practice.uncurry((a: Int) => (b: Int) => a + b)(1,2)

// test 2-5.
Practice.compose((b: Int) => b + 1, (a: Int) => a + 1)(1)

