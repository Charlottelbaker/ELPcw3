package Assign3.Syntax

import java.security.Identity
import scala.language.implicitConversions
import scala.collection.immutable.Map

/*======================================================================
  The first part of this file is support code, which you should not (and do not
  need to) change.
  ====================================================================== */

object Syntax {
  type Variable = String
  type Env = Map[Variable, Type]

  // ======================================================================
  // Expressions
  // ======================================================================
  sealed abstract class Expr

  // Unit
  case object Unit extends Expr

  // Arithmetic expressions
  case class Num(n: Integer) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(b: Boolean) extends Expr
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class Less(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr
  case class Print(e1: Expr, e2: Expr) extends Expr

  // Variables
  case class Var(x: Variable) extends Expr
  case class Loc(l: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  
  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, ty1: Type, ty2: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, x: Variable, ty: Type, e1: Expr, e2: Expr) extends Expr
  case class LetRec(f: Variable, x: Variable, ty1: Type, ty2: Type, e1: Expr, e2: Expr) extends Expr


  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class PmPair(x: Variable, y: Variable, e1: Expr, e2: Expr) extends Expr
  case class Fst(e: Expr) extends Expr
  case class Snd(e: Expr) extends Expr


  // Sums
  case class Left(e: Expr) extends Expr
  case class Right(e: Expr) extends Expr
  case class Case(e: Expr, x: Variable, e1: Expr, y: Variable, e2: Expr) extends Expr

  // (WHNF) Values (Call-by-need evaluates expressions to weak head normal form)
  abstract class Value extends Expr
  case object UnitV extends Value
  case class NumV(n: Integer) extends Value
  case class BoolV(b: Boolean) extends Value
  case class StringV(s: String) extends Value
  case class WorldV(s: String) extends Value
  case class PairV(e1: Expr, e2: Expr) extends Value
  case class LeftV(e: Expr) extends Value
  case class RightV(e: Expr) extends Value
  case class LambdaV(x: Variable, e: Expr) extends Value
  case class RecV(f: Variable, x: Variable, e: Expr) extends Value

  // ======================================================================
  // Types
  // ======================================================================
  sealed abstract class Type

  // Types
  case object TyInt extends Type
  case object TyBool extends Type
  case object TyString extends Type
  case object TyUnit extends Type
  case object TyWorld extends Type
  case object TyNothing extends Type
  case class TyBang(ty: Type) extends Type
  case class TyAFun(ty1: Type, ty2: Type) extends Type
  case class TyPair(ty1: Type, ty2: Type) extends Type
  case class TySum(ty1: Type, ty2: Type) extends Type


  // ======================================================================
  // Substitutions
  // ======================================================================

  // a class for generating fresh variables
  class SymGenerator {
    private var id = 0
    // generate a fresh variable from an existing variable
    def genVar(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
    // generate a fresh variable from nothing
    def freshVar(): Variable = {
      val fresh_s = "$" + id
      id = id + 1
      fresh_s
    }
  }

  // swap y and z in x
  def swapVar(x: Variable, y: Variable, z: Variable): Variable =
    if x == y then z else if x == z then y else x

  // a trait for substitutable things, e.g., expressions and types
  trait Substitutable[A] {
    // swap y and z in t
    def swap(t: A, y: Variable, z: Variable): A

    // subst x in t1 with t2, i.e., t1[t2/x]
    def subst(t1: A, t2: A, x: Variable): A
  }

    // ======================================================================
  // Capture-avoiding substitution
  // ======================================================================

  val generator = SymGenerator()

  object SubstExpr extends Substitutable[Expr] {
    // swap y and z in e
    def swap(e: Expr, y: Variable, z: Variable): Expr =
      def go(e: Expr): Expr = e match {
        // Values are closed
        case v: Value => v

        case Unit => Unit

        case Num(n) => Num(n)
        case Plus(e1, e2) => Plus(go(e1), go(e2))
        case Minus(e1, e2) => Minus(go(e1), go(e2))
        case Times(e1, e2) => Times(go(e1), go(e2))

        case Bool(b) => Bool(b)
        case Eq(e1, e2) => Eq(go(e1), go(e2))
        case Less(e1, e2) => Less(go(e1), go(e2))
        case IfThenElse(e, e1, e2) => IfThenElse(go(e), go(e1), go(e2))

        case Str(s) => Str(s)
        case Length(e) => Length(go(e))
        case Index(e1, e2) => Index(go(e1), go(e2))
        case Concat(e1, e2) => Concat(go(e1), go(e2))
        case Print(e1, e2) => Print(go(e1), go(e2))

        case Var(x) => Var(swapVar(x,y,z))
        case Loc(l) => Loc(l)
        case Let(x,e1,e2) => Let(swapVar(x,y,z),go(e1),go(e2))
  
        case Pair(e1,e2) => Pair(go(e1),go(e2))
        case PmPair(x1,x2,e1,e2) =>
          PmPair(swapVar(x1,y,z),swapVar(x2,y,z),go(e1),go(e2))
        case Fst(e) => Fst(go(e))
        case Snd(e) => Snd(go(e))

        case Lambda(x,ty,e) => Lambda(swapVar(x,y,z), ty, go(e))
        case Rec(f,x,ty1,ty2,e) => Rec(swapVar(f,y,z),swapVar(x,y,z),ty1,ty2,go(e))
        case Apply(e1,e2) => Apply(go(e1),go(e2))
        case LetFun(f,x,ty,e1,e2) => LetFun(swapVar(f,y,z),swapVar(x,y,z),ty,go(e1),go(e2))
        case LetRec(f,x,ty1,ty2,e1,e2) => LetRec(swapVar(f,y,z),swapVar(x,y,z),ty1,ty2,go(e1),go(e2))

        case Left(e) => Left(go(e))
        case Right(e) => Right(go(e))
        case Case(e, x1, e1, x2, e2) =>
          Case(go(e), swapVar(x1,y,z), go(e1), swapVar(x2,y,z), go(e2))
        }
      go(e)

    ////////////////////
    // EXERCISE 1     //
    ////////////////////
    def subst(e: Expr, ex: Expr, x: Variable): Expr = e match {
        // Values are closed 
        case v: Value => v
  
        // BEGIN ANSWER
        case _ => sys.error("todo")

          // END ANSWER
    }
  }


}


