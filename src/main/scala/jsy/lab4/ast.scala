/**
 * Version History:
 *   0.2 (09/18/2012): Changed printing of AST nodes.
 *   0.1 (09/07/2012): Initial release.
 */
package jsy.lab4

import scala.util.parsing.input.Positional
import scala.collection.immutable.TreeMap

/**
 * @author Bor-Yuh Evan Chang
 */
object ast {
  sealed abstract class Expr extends Positional
  
  /* Variables */
  case class Var(x: String) extends Expr
  
  /* Declarations */
  case class Decl(mode: Mode, x: String, e1: Expr, e2: Expr) extends Expr
  
  /* Literals and Values*/
  case class N(n: Double) extends Expr
  case class B(b: Boolean) extends Expr
  case object Undefined extends Expr
  case class S(s: String) extends Expr
  
  /* Unary and Binary Operators */
  case class Unary(uop: Uop, e1: Expr) extends Expr
  case class Binary(bop: Bop, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Uop
  
  case object Neg extends Uop /* - */
  case object Not extends Uop /* ! */

  sealed abstract class Bop
  
  case object Plus extends Bop /* + */
  case object Minus extends Bop /* - */
  case object Times extends Bop /* * */
  case object Div extends Bop /* / */
  case object Eq extends Bop /* === */
  case object Ne extends Bop /* !=== */
  case object Lt extends Bop /* < */
  case object Le extends Bop /* <= */
  case object Gt extends Bop /* > */
  case object Ge extends Bop /* >= */
  
  case object And extends Bop /* && */
  case object Or extends Bop /* || */
  
  case object Seq extends Bop /* , */
  
  /* Intraprocedural Control */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr
  
  /* Functions */
  case class Function(p: Option[String], params: List[(String,MTyp)], tann: Option[Typ], e1: Expr) extends Expr
  case class Call(e1: Expr, args: List[Expr]) extends Expr

  /* I/O */
  case class Print(e1: Expr) extends Expr

  /* Objects */
  case class Obj(fields: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr

  /* Types */
  sealed abstract class Typ
  case object TNumber extends Typ
  case object TBool extends Typ
  case object TString extends Typ
  case object TUndefined extends Typ
  case class TFunction(params: List[(String,MTyp)], tret: Typ) extends Typ
  case class TObj(tfields: Map[String, Typ]) extends Typ

  /* Parameter Modes */
  sealed abstract class Mode
  case object Const extends Mode
  case object Name extends Mode

  /* Parameter Types */
  case class MTyp(m: Mode, t: Typ)

  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case N(_) | B(_) | Undefined | S(_) | Function(_, _, _, _) => true
    case Obj(fields) if (fields forall { case (_, ei) => isValue(ei) }) => true
    case _ => false
  }
  
  /*
   * Pretty-print values.
   * 
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(p, _, _, _) =>
        "[Function%s]".format(p match { case None => "" case Some(s) => ": " + s })
      case Obj(fields) =>
        val pretty_fields =
          fields map {
            case (f, S(s)) => f + ": '" + s + "'"
            case (f, v) => f + ": " + pretty(v)
          } reduceRight {
            (s, acc) => s + ",\n  " + acc
          }
        "{ %s }".format(pretty_fields)
    }
  }
  
  /*
   * Pretty-print types.
   * 
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(t: Typ): String = t match {
    case TNumber => "number"
    case TBool => "bool"
    case TString => "string"
    case TUndefined => "Undefined"
    case TFunction(params, tret) => {
      val pretty_params =
        params map { case (x,t) => "%s: %s".format(x, pretty(t)) } reduceRight {
          (s, acc) => s + ", " + acc
        }
      "(%s) => %s".format(pretty_params, pretty(tret))
    }
    case TObj(tfields) =>
      val pretty_fields =
        tfields map { case (f,t) => "%s: %s".format(f, pretty(t)) } reduceRight {
          (s, acc) => s + "; " + acc
        }
      "{ %s }".format(pretty_fields)
  }

  def pretty(mty: MTyp): String = mty match {
    case MTyp(Const, ty) => s"${pretty(ty)}"
    case MTyp(Name, ty) => s"name ${pretty(ty)}"
  }

  /* Get the free variables of e. */
  def freeVars(e: Expr): Set[String] = e match {
    case Var(x) => Set(x)
    case Decl(_, x, e1, e2) => freeVars(e1) | (freeVars(e2) - x)
    case Function(p, params, _, e1) => freeVars(e1) -- (params map { _._1 }) -- p
    case N(_) | B(_) | Undefined | S(_) => Set.empty
    case Unary(_, e1) => freeVars(e1)
    case Binary(_, e1, e2) => freeVars(e1) | freeVars(e2)
    case If (e1, e2, e3) => freeVars(e1) | freeVars(e2) | freeVars(e3)
    case Call(e1, args) => freeVars(e1) | args.foldLeft(Set.empty: Set[String]){ (acc: Set[String], ei) => acc | freeVars(ei) }
    case Print(e1) => freeVars(e1)
    case Obj(fields) => fields.foldLeft(Set.empty: Set[String]){ (acc: Set[String], p: (String, Expr)) => acc | freeVars(p._2) }
    case GetField(e1, _) => freeVars(e1)
  }
  
  /* Check closed expressions. */
  def closed(e: Expr): Boolean = freeVars(e).isEmpty
   
  /*
   * Dynamic Type Error exception.  Throw this exception to signal a dynamic
   * type error.
   * 
   *   throw DynamicTypeError(e)
   * 
   */
  case class DynamicTypeError(e: Expr) extends Exception {
    override def toString = Parser.formatErrorMessage(e.pos, "DynamicTypeError", "in evaluating " + e)
  }
  
  /*
   * Static Type Error exception.  Throw this exception to signal a static
   * type error.
   * 
   *   throw StaticTypeError(tbad, esub, e)
   * 
   */
  case class StaticTypeError(tbad: Typ, esub: Expr, e: Expr) extends Exception {
    override def toString =
      Parser.formatErrorMessage(esub.pos, "StaticTypeError", "invalid type %s for sub-expression %s in %s".format(pretty(tbad), esub, e))
  }
  
   /*
   * Stuck Error exception.  Throw this exception to signal getting
   * stuck in evaluation.  This exception should not get raised if
   * evaluating a well-typed expression.
   * 
   *   throw StuckError(e)
   * 
   */
  case class StuckError(e: Expr) extends Exception {
    override def toString = Parser.formatErrorMessage(e.pos, "StuckError", "in evaluating " + e)
  }
  
}