package jsy.lab4

import jsy.lab4.ast._
import jsy.lab4.Parser.parse
import jsy.util.JsyApplication

trait Lab4Like { a: JsyApplication =>

  def compressRec[A](l: List[A]): List[A]
  def compressFold[A](l: List[A]): List[A]
  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A]

  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) =>
        if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    }

    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      }
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A
  def sum(t: Tree): Int
  def treeFromList(l: List[Int]): Tree
  def strictlyOrdered(t: Tree): Boolean

  /* Type Inference */

  type TEnv = Map[String, Typ]
  val empty: TEnv = Map()
  def lookup(env: TEnv, x: String): Typ = env(x)
  def extend(env: TEnv, x: String, t: Typ): TEnv = {
    env + (x -> t)
  }

  def typeof(env: TEnv, e: Expr): Typ

  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean

  def iterate(e0: Expr)(body: (Expr, Int) => Option[Expr]): Expr
  def substitute(e: Expr, v: Expr, x: String): Expr
  def step(e: Expr): Expr

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    }
    val t = typeof(empty, e)
    if (debug) {
      println("Type: " + pretty(t))
    }
    t
  }

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    require(closed(e), s"iterateStep: e ${e} not closed")
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = iterate(e) { (e: Expr, n: Int) =>
      if (debug) { println(s"Step $n: $e") }
      if (isValue(e)) None else Some(step(e))
    }
    if (debug) { println("Value: " + v) }
    v
  }

  /** Interface to take a small-step from a string. This is convenient for unit testing. */
  def oneStep(s: String): Expr = step(parse(s))

  /** Interface to run your small-step interpreter from a string. This is convenient for unit testing. */
  def iterateStep(s: String): Expr = iterateStep(parse(s))

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }

    val e1 =
      handle(None: Option[Expr]) {
        Some {
          Parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    val welltyped = handle(false) {
      println("# Type checking ...")
      val t = inferType(e1)
      println("## " + pretty(t))
      true
    }
    if (!welltyped) return

    handle() {
      println("# Stepping ...")
      def loop(e: Expr, n: Int): Expr = {
        println("## %4d: %s".format(n, e))
        if (isValue(e)) e else loop(step(e), n + 1)
      }
      val v1 = loop(e1, 0)
      println(pretty(v1))
    }
  }

}
