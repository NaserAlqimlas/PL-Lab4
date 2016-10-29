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

  /* Environments */
  def empty[A]: Map[String,A] = Map.empty
  def lookup[A](env: Map[String,A], x: String): A = env(x)
  def extend[A](env: Map[String,A], x: String, a: A): Map[String,A] = {
    env + (x -> a)
  }

  /* Type Inference */
  type TEnv = Map[String, Typ]
  def typeof(env: TEnv, e: Expr): Typ

  /* Step */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean
  def iterate(e0: Expr)(body: (Expr, Int) => Option[Expr]): Expr
  def rename(e: Expr)(fresh: String => String): Expr
  def substitute(e: Expr, v: Expr, x: String): Expr
  def isRedex(mode: Mode, e: Expr): Boolean
  def step(e: Expr): Expr

  /** Interface to run your type checker. */
  def inferType(e: Expr): Typ = {
    val t = typeof(empty, e)
    println(s"## ${e} : ${pretty(t)}")
    t
  }

  /** Interface to run your small-step interpreter
    * and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Expr = {
    //require(closed(e), "input Expr to iterateStep is not closed: free variables: %s".format(freeVars(e)) )
    val v = iterate(e) { (e: Expr, n: Int) =>
      if (Some(n) == maxSteps) throw TerminationError(e, n)
      if (isValue(e)) None else {
        println("## step %4d: %s".format(n, e))
        Some(step(e))
      }
    }
    println(s"## value: %s".format(v))
    v
  }

  /** Interface to take a small-step from a string. This is convenient for unit testing. */
  def oneStep(s: String): Expr = step(parse(s))

  /** Interface to run your small-step interpreter from a string. This is convenient for unit testing. */
  def iterateStep(s: String): Expr = iterateStep(parse(s))

  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("# ============================================================")
      println("# File: " + file.getName)
      println("# Parsing ...")
    }

    val exprin =
      handle(None: Option[Expr]) {
        Some {
          Parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    val expr = exprin

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Type checking %s ...".format(expr))
    }

    val welltyped = handle(false) {
      val t = inferType(expr)
      true
    }
    if (!welltyped) return

    if (debug) {
      println("# ------------------------------------------------------------")
      println("# Stepping ...".format(expr))
    }

    handle() {
      val v = iterateStep(expr)
      println(pretty(v))
    }
  }

}
