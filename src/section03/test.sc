import section03._

List(1,2,3)
Cons(1, List(2,3,4))
List.sum(List(1,2,3))

Practice.x

Practice.tail((List(1,2,3)))

Practice.setHead(5, List(1,2,3,4))

Practice.drop(List(1,2,3,4,5), 3)

Practice.dropWhile(List(1,2,3,4,5), (x: Int) => {x < 2})

Practice.init(List(1,2,3,4,5))

List.foldRight(List(1,2,3), 0) (_ + _)
List.foldRight(List(1,2,3), 1.0) (_ * _)
List.product2(List(1,2,3,0,4,5))
List.foldRight(List(1,2,3), Nil: List[Int]) (Cons(_,_))
Practice.length(List(2,3,4,5))