package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.1
  val freds_31 = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("set head of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // 3.7
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  // 3.8
  def freds_38(ns: List[Int]) =
    foldRight(ns, Nil:List[Int])(Cons(_,_))

  // 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,  t) => foldLeft(t, f(z,h))(f)
  }

  // 3.11.a
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  // 3.11.b
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.11.c
  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())( (acc, h) => Cons(h,acc))

  // 3.13
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(l), z) ((h, t) => f(t, h))
  
  // 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2) ( (acc, h) => Cons(h, acc) )

  // 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]()) ( append )

  // 3.16
  def add1(list: List[Int]): List[Int] = 
    foldRight(list, List[Int]()) ( (head, tail) => Cons(head+1, tail))

  // 3.17
  def doubleToString(list: List[Double]): List[String] = 
    //foldRight(list, List[String]()) ( (head, tail) => Cons('-' + head.toString + '-', tail) )
    foldRight(list, Nil:List[String])((h,t) => Cons(h.toString,t))

  // 3.18
  def map[A,B](list: List[A])(func: A => B): List[B] = 
    foldRight(list, Nil:List[B]) ( (h, t) => Cons(func(h), t) )

  // 3.19
  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    foldRight(list, Nil:List[A]) ( (h, t) => {
      if (func(h)) Cons(h, t)
      else t
    })

  // 3.20
  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    //foldRight(list, Nil:List[B]) ( (h, t) => append(func(h), t) )
    concat(map(list)(func))

  // 3.21
  def filter2[A](list: List[A])(func: A => Boolean): List[A] =
    flatMap(list)( (h) =>
      if (func(h)) List(h)
      else Nil
    )

  // 3.22
  def sumLists(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, sumLists(t1,t2))
  }

  // 3.23
  def zipWith[A, B, C](listA: List[A], listB: List[B])(func: (A, B) => C): List[C] = (listA,listB) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(func(h1,h2), zipWith(t1,t2)(func))
  }

  // 3.24
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }



}
