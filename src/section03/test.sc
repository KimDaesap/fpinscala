import section03._

List(1,2,3)
Cons(1, List(2,3,4))
List.sum(List(1,2,3))

List.tail((List(1,2,3)))

List.setHead(5, List(1,2,3,4))

List.drop(List(1,2,3,4,5), 3)

List.dropWhile(List(1,2,3,4,5), (x: Int) => {x < 2})

List.init(List(1,2,3,4,5))

List.foldRight(List(1,2,3), 0) (_ + _)
List.foldRight(List(1,2,3), 1.0) (_ * _)
List.product2(List(1,2,3,0,4,5))
List.foldRight(List(1,2,3), Nil: List[Int]) (Cons(_,_))
List.length(List(2,3,4,5))
List.foldRight(List(1,2,3,4,5), 0) ((x,y) => x + y)
List.foldLeft(List(1,2,3,4,5), 0) ((x,y) => x + y)

List.filterViaFlatMap(List(1,2,3,4,5))(a => a > 3)
