/*
 * CSCI 3155
 *
 * Meeting 15-16: Collections and callbacks: Higher-order functions.
 */

/***** Functions *****/

// Remember: "Functions are values!"

val incr0: Int => Int = { n => n + 1 }

// Scala style note: it is common and a recommended style to enclose function literals
// with braces { }. Remember that braces { } can be used like parentheses ( ) on
// expressions.
//
// The following are the same in the abstract syntax:
val incr1: Int => Int = { n => n + 1 }
val incr2: Int => Int = (n => n + 1)
val incr3: Int => Int = n => n + 1

incr0(10)
incr1(10)
incr2(10)
incr3(10)

// Recall that because functions are values, they can be arguments or return values.

// A function that returns another function behaves like a multi-parameter function. This
// is called currying.

def plus(x: Int, y: Int): Int = x + y

def curriedPlus0(x: Int): Int => Int = { y => x + y }

// Since currying is such a common thing to do, Scala has some syntactic sugar for it. The
// following function curriedPlus1 is equivalent to curriedPlus0.
def curriedPlus1(x: Int)(y: Int): Int = x + y

// And the versions above are the same as this function literal binding:
val curriedPlus2: Int => Int => Int = { x => { y => x + y } }

// Why curry? Currying enables partial application.
val incr4 = curriedPlus0(1)

incr4(10)

// Sometimes partial application is simply for defining new functions in terms of others
// in a compact manner. Other times, partial application enables some non-trivial partial
// computation.

// Silly example: add m to n!.
def addfactorial(n: Int): Int => Int = {
  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n - 1)
  }
  val nth = factorial(n)
  (m: Int) => nth + m
}

// 10! computed once and then reused
val tenfactorialplus = addfactorial(10)
tenfactorialplus(11)
tenfactorialplus(12)

/***** Lists *****/

object MyList {
  /* The Scala List library is defined with essentially these constructors.
   *
   * This means that one can use pattern matching to recurse over lists.
   *
   * We can view :: (pronounced "cons") as the prepend-an-element operator.  What
   * is this data structure that you have almost certainly seen before in a data
   * structures course? This is a "functional singly-linked list." Nil is the
   * empty list, while the tail field in the a :: node is the "next pointer."
   * This structure is "functional" because the tail field is immutable.
   */
  sealed abstract class List[A] // [A] is a type parameter.
  case object Nil extends List[Nothing]
  case class ::[A](head: A, tail: List[A]) extends List[A]
}

/* The List(...) constructor is simply delegates calls to the primitive
 * :: and Nil constructors. */
List()
List(1, 2, 3)

Nil
1 :: (2 :: (3 :: Nil))

val l0 = List(1,2,3)
val l1 = List(4,5,6)
7 :: l0  // :: : (Int, List[Int]) => List[Int]
         // "cons": prepend element in a linked list (like .next)

val empty = Nil

empty == Nil
empty.isEmpty

l0.head
l0.tail

/* Length of a list.
 *
 * [A] is a type parameter.
 */
def len0[A](l: List[A]): Int =
  if (l == Nil) 0 else 1 + len0(l.tail)
  
def len1[A](l: List[A]): Int = l match {
  case Nil => 0
  case _ :: t => 1 + len1(t)
}

// Both versions above compute the length of the list that
// is also a library method.
def len3[A](l: List[A]): Int = l.length

// Note that the :: is usually written infix, but it is not special. The
// following also works.
def len2[A](l: List[A]): Int = l match {
  case Nil => 0
  case ::(_, t) => 1 + len2(t)
}

// Let's try it!
len0(l0)
len1(l0)
len2(l0)
len3(l0)

/***** Parametric Polymorphism *****/
def idInt(x: Int): Int = x
def idBoolean(x: Boolean): Boolean = x
idInt(3)
idBoolean(true)

def id[A](x: A): A = x /* id : for all A. A => A */
id[Boolean](true)
id(true) /* via type inference */

def first[A,B](p: (A,B)): A = {
  val (x, _) = p
  x
}

/***** Lists *****/
/* EXERCISE: List append.
 *
 * Returns a list with the elements of xl followed by the elements of yl.
 */
def append[A](xl: List[A], yl: List[A]): List[A] = xl match {
  case Nil => yl
  case h :: t => h :: append(t, yl)
}

append(l0, l1)

// ::: is "append" in the Scala library, so let's test.
assert(append(l0, l1) == l0 ::: l1)

// A buggy append. What does buggyAppend do?
def buggyAppend[A](xl: List[A], yl: List[A]): List[A] = xl match {
  case Nil => yl
  case h :: t => buggyAppend(t, h :: yl)
}

buggyAppend(l0, l1)

/* List reverse.
 * 
 * Complexity: O(n^2) where n is the length of l.
 */
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case h :: t => append(reverse(t), List(h))
}

/* EXERCISE: Write the linear time reverse.
 * HINT: You'll need a helper function and it will be tail recursive.
 */
def reverseAcc[A](l: List[A]): List[A] = {
  def rev(l: List[A], acc: List[A]): List[A] = l match {
    case Nil => acc
    case h :: t => rev(t, h :: acc)
  }
  rev(l, Nil)
}

// A execution trace:
//
// l = 1 :: 2 :: Nil
//
// rev(1 :: 2 :: Nil, Nil) -->*
// rev(2 :: Nil, 1 :: Nil) -->*
// rev(Nil, 2 :: 1 :: Nil) -->*
// 2 :: 1 :: Nil

// Observe that rev is exactly buggyAppend. The specification of rev (or buggyAppend) is
// that it returns the reverse of the its first argument l (or xl) followed by its second argument
// acc (or yl) appended.

// **Key Idea**
//
// Previously, our discussion about tail recursion was simply about efficiency because the
// operators we considered were commutative (e.g., + or * on integers). Now, with a
// non-commutative operator like ::, we see that there is something more.
//
// The intuition is that the accumulator parameter acc in rev enables us to "do something"
// as we "recurse down" the list. And the stack in a non-tail recursive function enables us
// to "do something" as we "return up".

/***** List and Higher-Order Functions. *****/

// Perhaps the most fundamental operation over a collection is iterating over its elements.

/*** Map and Filter ***/

def incr(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case h :: t => (h + 1) :: incr(t)
}

def double(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case h :: t => (h * 2) :: incr(t)
}

/* EXERCISE: Write a function that applies a function f to each element of a list l. */
def mymap[A, B](l: List[A])(f: A => B): List[B] = l match {
  case Nil => Nil
  case h :: t => f(h) :: mymap(t)(f)
}

// The main advantage of iteration functions like mymap is that they abstract the recusion over
// the list and allow the client programmer to focus on what to do for each element of the
// list via the callback "f".

def incrList1(l: List[Int]): List[Int] = {
  def plusOne(x: Int): Int = x + 1
  mymap(l)(plusOne)
} 

val i1 = incrList1(l0)

// Combined with "functions are values," we do not need to name the "callback". This makes mymap
// like a custom "looping" construct for lists.

val i2 = mymap(l0)(x => x + 1)
val i3 = mymap(l0) { x => x + 1 } // the braces really makee mymap look like a custom loop for C/Java programmers

// In the Scala library, map is a method on List objects.
val i4 = l0.map(x => x + 1)
val i5 = l0.map { x => x + 1 }

// A note about Scala infix operators: any binary method (a method with the receiver and one additional
// formal parameter can be treated as an infix operator. There's nothing special symbols.
//
// For example, + is simply a method on Int (and other numeric classes).
3 + 4
3.+(4)
case class MyInt(n: Int) {
  def +(that: MyInt) = this.n + that.n
}
MyInt(3) + MyInt(4)

// Thus, map is often written infix.
val i6 = l0 map { x => x + 1 }

// The infix notation makes chaining clear.
val di1 = l0 map { x => x + 1 } map { x => x * 2 }

// There's a shorthand syntax for creating function literals with _. In general, I recommend avoiding
// this shorthand, as it can be quite hard to read (except perhaps for the simplest function literals
// as we have here).
val i7 = l0 map { _ + 1 }

// Mapping is part of "comprehensions" (e.g., something like set comprehensions in standard set notation
// { f(x) | x in L }.
//
// Scala has way to express map in this way. Any object with a method called "map" can be used in a
// for-yield expression.
val i8 = for (x <- l0) yield x + 1

// Sometimes, we want to pattern match in the argument.

val l3 = List(None, Some(3), Some(4), None)
val i9 = l3 map { xopt => xopt match { case None => 0 case Some(x) => x + 1 } }

// We can drop the match part to get the same behavior. [In actuality, there are semantics are
// different. Read about "partial functions" versus "functions" in the Scala book.
val i10 = l3 map { case None => 0 case Some(x) => x + 1 }

// Another common List operation is to filter a list.

val o1 = l0 filter { x => x % 2 == 1 }
val o2 = for (x <- l0 if x % 2 == 1) yield x

// And filter can easily be combined with map.

val oi1 = l0 filter { x => x % 2 == 1} map { x => x + 1 }
val oi2 = for (x <- l0 if x % 2 == 1) yield x + 1

/*** Fold ***/

def addList(l: List[Int]): Int = l match {
  case Nil => 0
  case h :: t => h + addList(t) 
}

def multList(l: List[Int]): Int = l match {
  case Nil => 1
  case h :: t => h * multList(t)
}

// Wow, this is really repetitive. Could we write a function that abstracts the recursion down
// a list. Yes, this is called foldRight!

/* EXERCISE
 *
 * Given l = List(a, b, c, ..., d)
 * 
 * myfoldRight(l)(z)(f) returns
 *   f(a, f(b, f(c, ... f(d, z))))
 */
def myfoldRight[A,B](l: List[A])(z: B)(op: (A, B) => B): B = l match {
  case Nil => z
  case h :: t => op(h, myfoldRight(t)(z)(op))
}

def addList1(l: List[Int]): Int =
  myfoldRight(l)(0) { (h, acc) => h + acc }
  // We name the parameters to the myFoldRight callback h for the head element
  // and acc for the accumulated result of the recursion.

val al0 = addList(l0)
val al1 = addList1(l0)

// Again, the Scala standard library defines foldRight as a method on Lists.
val al2 = l0.foldRight(0) { (h, acc) => h + acc }

// The Scala also has a kind of cryptic operator for foldRight (not necessarily recommended), that is,
// :\ is the same as foldRight.
val al3 = (l0 :\ 0) { (h, acc) => h + acc }
val al4 = (l0 foldRight 0) { (h, acc) => h + acc }

// And now we can write perform multList in a one liner
val ml1 = l0.foldRight(1) { (x, acc) => x * acc }

// We can also consider more specific cases in the op callback
val ml2 = l0.foldRight(1) {
  case (0, _) | (_, 0) => 0
  case (x, acc) => x * acc
}

// foldRight abstracts structural recursion over Lists

/* EXERCISE: Write map with foldRight */
def mymap2[A, B](l: List[A])(f: A => B): List[B] =
  l.foldRight(Nil: List[B]) { (h, acc) => f(h) :: acc }

/* EXERCISE: Write append with foldRight */
def append2[A](xl: List[A], yl: List[A]): List[A] =
  xl.foldRight(yl) { (h, acc) => h :: acc }

// What about the O(n) reverse? The tail recursive rev helper function from above
// is an iteration that we want abstract. It's foldLeft!

/* EXERCISE
 *
 * Given l = List(a, b, c, ..., d)
 *
 * myfoldLeft(l)(z)(f) returns
 *   f(... f(f(f(z, a), b), c), d)
 */
def myfoldLeft[A,B](l: List[A])(acc: B)(op: (B, A) => B): B = l match {
  case Nil => acc
  case h :: t => myfoldLeft(t)(op(acc, h))(op)
}

// Because * on integers is commutative, we get the result for multiplying a list of integers
// from left-to-right.
val ml3 = myfoldLeft(l0)(1) { (acc, h) => acc * h }

// Equivalently using the Scala library
val ml4 = l0.foldLeft(1) { (acc, h) => acc * h }
val ml5 = (1 /: l0) { (acc, h) => acc * h }

/* EXERCISE: Write reverse using foldLeft */
def reverse2[A](l: List[A]): List[A] =
  l.foldLeft(Nil: List[A]) { (acc, x) => x :: acc }

val r1 = reverse2(l0)

/* Heard of Google's MapReduce? Reduce = Fold. */

