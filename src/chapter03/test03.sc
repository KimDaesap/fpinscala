import chapter03._

// List
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
List.addList(List(1,2,3,4), List(4,5,6))

// Tree
val tree1 = Branch(Leaf(1), Leaf(2))
val tree2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

Tree.size(tree1)
Tree.size(tree2)

Tree.depth(tree1)
Tree.depth(tree2)

Tree.fold(tree1)(v => v)((l,r) => l + r)
Tree.fold(tree2)(v => v)((l,r) => l + r)
