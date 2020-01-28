package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//Tree.size(Branch(Branch(Leaf("a"),Leaf("b")),Branch(Leaf("c"), Leaf("d"))))
//Tree.size(Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3), Leaf(4))))

object Tree {

	// 3.25
	def size[A](tree: Tree[A]): Int = tree match {
	  case Leaf(_) => 1
	  case Branch(l, r) => size(l) + size(r)
	}

	// 3.26
	def maximum(tree: Tree[Int]): Int = tree match {
	  case Leaf(n) => n
	  case Branch(l, r) => maximum(l).max(maximum(r))
	}

	// 3.27
	def depth[A](tree: Tree[A]): Int = tree match {
	  case Leaf(n) => 0
	  case Branch(l, r) => 1 + depth(l) max depth(r)
	}

	// 3.28
	def map[A,B](tree: Tree[A])(func: A => B): Tree[B] = tree match {
		case Leaf(n) => Leaf(func(n))
		case Branch(l, r) => Branch(map(l)(func), map(r)(func))
	}

	// 3.29.a
	def fold[A,B](tree: Tree[A])(fa: A => B)(fb: (B,B) => B): B = tree match {
		case Leaf(a) => fa(a)
		case Branch(l, r) => fb(fold(l)(fa)(fb), fold(r)(fa)(fb))
	}

	// 3.29.b
	def size2[A](tree: Tree[A]): Int =
		fold(tree)(a => 1)(_+_)

	// 3.29.c
	def maximum2(tree: Tree[Int]): Int =
		fold(tree)(n => n)(_ max _)

	// 3.29.d
	def depth2[A](tree: Tree[A]): Int =
		fold(tree)(a => 0)(_ max _ + 1)

	// 3.29.e
	def map2[A,B](tree: Tree[A])(func: A => B): Tree[B] =
		fold(tree)(a => Leaf(func(a)):Tree[B])((l, r) => Branch(l, r))


}

// x
// | |
// 1 2

// fold(Branch(Leaf(1), Leaf(2))) (fa) (fb)
// fb(fold(Leaf(1) (fa) (fb), fold(Leaf(2) (fa) (fb))))
// fb (fa(1), fa(2))