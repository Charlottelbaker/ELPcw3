package Assign3.Typer

import Assign3.Syntax.Syntax._

object Typer {
  // ======================================================================
  // Part 1: Typechecking
  // ======================================================================

  val generator = SymGenerator()

  ////////////////////
  // EXERCISE 2     //
  ////////////////////



  def normTy(ty: Type): Type = 
    // BEGIN ANSWER
    sys.error("todo") def normBang(t: Type): Type = t match { // this happens when a bang is found 
      case TyInt | TyBool | TyString | TyUnit => t // remove bang from basic type 
      case TyNothing => TyNothing // remove bang from nothing type 
      case TyBang(t1) => TyBang(normTy(t1)) // leave one of the bangs to remove double bang
      case TyPair(t1, t2) => TyPair(TyBang(normTy(t1)), TyBang(normTy(t2))) // push bang onto inner 
      case TySum(t1, t2) => TySum(TyBang(normTy(t1)), TyBang(normTy(t2)))
      case _ => TyBang(t)
    }

    ty match {
      case TyInt | TyBool | TyString | TyUnit | TyWorld | TyNothing => ty // no bangs found 
      case TyPair(t1, t2) => TyPair(normTy(t1), normTy(t2)) // check in pair for bangs 
      case TySum(t1, t2) => TySum(normTy(t1), normTy(t2)) // check in sum for bang
      case TyAFun(t1, t2) => TyAFun(normTy(t1), normTy(t2)) // check in function for bang
      case TyBang(t) => normBang(normTy(t)) // bang found 
    }
    // END ANSWER

  ////////////////////
  // EXERCISE 3     //
  ////////////////////

  def subtype(ty1: Type, ty2: Type): Boolean =
    // BEGIN ANSWER 
    ty1 match {
      case TyInt => ty2 match {
        case TyInt => true
        case _ => false
      }
      case TyBool => ty2 match {
        case TyBool => true
        case _ => false
        
      }
      case TyString => ty2 match {
        case TyString => true
        case _ => false
        
      }
      case TyUnit => ty2 match {
        case TyUnit => true
        case _ => false
        
      }
      case TyWorld => ty2 match {
        case TyWorld => true
        case _ => false
        
      }
      case TyBang(t1) => ty2 match {
        case TyBang(t2) => subtype(t1, t2) // case when both have bang
        case _ => subtype(t1, ty2) // case where only 1 has bang
      }
      case TySum(ty1_a, ty1_b) => ty2 match {
        case TySum(ty2_a, ty2_b) => subtype(ty1_a, ty2_a) && subtype(ty1_b, ty2_b)
        case _ => false
      } 
       case TyPair(ty1_a, ty1_b) => ty2 match {
        case TyPair(ty2_a, ty2_b) => subtype(ty1_a, ty2_a) && subtype(ty1_b, ty2_b)
        case _ => false
      } 
       case TyAFun(ty1_a, ty1_b) => ty2 match {
        case TyAFun(ty2_a, ty2_b) => subtype(ty2_a, ty1_a) && subtype(ty1_b, ty2_b)
        case _ => false
      } 
      case TyNothing => true
      case _ => false  
    }
    // END ANSWER


  ////////////////////
  // EXERCISE 3     //
  ////////////////////


  // The following functions should define meet and join as described in the
  // handout, only considering cases for normal forms.
  // The helper functions tyMeetNorm and tyJoinNorm call these
  // using normalization first, if needed.
  def tyMeet(ty1 : Type, ty2: Type): Type = (ty1, ty2) match {
    // BEGIN ANSWER
     case (TyInt, TyInt) => TyInt
    case (TyBool, TyBool) => TyBool
    case (TyUnit, TyUnit) => TyUnit
    case (TyString, TyString) => TyString
    case (TyWorld, TyWorld) => TyWorld
    case (TyBang(t), TyBang(s)) => TyBang(tyMeet(t,s))
    case (TyPair(a,b), TyPair(a1,b1)) => TyPair((tyMeet(a,a1), tyMeet(b,b1)))
    case (TySum(a,b), TySum(a1,b1)) => TySum((tyMeet(a,a1), tyMeet(b,b1)))
    case (TyAFun(a,b), TyAFun(a1,b1)) => TyAFun((tyJoin(a,a1), tyMeet(b,b1)))
    case (TyBang(a), b) | (a, TyBang(b)) => TyBang(tyMeet(a,b))
    case (TyNothing, a) | (a, TyNothing) => TyNothing
    case _ => sys.error("No join for those types")
    // END ANSWER
  }

  def tyJoin(ty1 : Type, ty2: Type): Type = (ty1, ty2) match {
    // BEGIN ANSWER
    case (TyInt, TyInt) => TyInt
    case (TyBool, TyBool) => TyBool
    case (TyUnit, TyUnit) => TyUnit
    case (TyString, TyString) => TyString
    case (TyWorld, TyWorld) => TyWorld
    case (TyBang(t), TyBang(s)) => TyBang(tyJoin(t,s))
    case (TyPair(a,b), TyPair(a1,b1)) => TyPair((tyJoin(a,a1), tyJoin(b,b1)))
    case (TySum(a,b), TySum(a1,b1)) => TySum((tyJoin(a,a1), tyJoin(b,b1)))
    case (TyAFun(a,b), TyAFun(a1,b1)) => TyAFun((tyMeet(a,a1), tyJoin(b,b1)))
    case (TyBang(a), b) | (a, TyBang(b)) => tyJoin(a,b)
    case (TyNothing, a) | (a, TyNothing) => a
    case _ => sys.error("No join for those types")
    // END ANSWER
  }


  def tyMeetNorm(ty1 : Type, ty2: Type): Type =
    tyMeet (normTy(ty1), normTy(ty2))
  def tyJoinNorm(ty1 : Type, ty2: Type): Type =
    tyJoin (normTy(ty1), normTy(ty2))
  
  def isEqType(ty: Type): Boolean = ty match {
    case TyUnit | TyInt | TyString | TyBool => true
    case _ => false
  }
  
  def tyIsUnrestricted(ty : Type) : Boolean = subtype(ty, TyBang(ty))
  def tyIsRestricted(ty : Type) : Boolean = ! tyIsUnrestricted(ty)
  def envIsUnrestricted(env : Env) : Boolean = env.forall((_,ty) => tyIsUnrestricted(ty))
  def envIsRestricted(env : Env) : Boolean = ! envIsUnrestricted(env)
  def envIntersect(ctx1 : Env, ctx2 : Env) : Env = 
    ctx1.keySet.intersect(ctx2.keySet).map(k => k -> ctx1(k)).toMap

  ////////////////////
  // EXERCISE 5     //
  ////////////////////

  // checking mode
  def tyCheck(ctx1: Env, e: Expr, ty1: Type): Env =
    // BEGIN ANSWER
   val (ctx2, tyActual) = tyInfer(ctx1, e)
      if (tySub(tyActual, tyExpected))
        ctx2
      else
        sys.error("Type mismatch")
    }
    // END ANSWER

  
  // inference mode
  def tyInfer(ctx: Env, e: Expr): (Env, Type) = e match {
    // Values
    case v:Value => sys.error("tyInfer: values should not appear at this stage")
    // BEGIN ANSWER

    // Arithmetic expressions
    case Num(_) => TyInt
    case Plus(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyInt) 
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyInt) 
      (ctx2, TyInt)
    
      case Minus(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyInt) 
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyInt) 
      (ctx2, TyInt)
       
      case Times(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyInt) 
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyInt) 
      (ctx2, TyInt)
       

    // Booleans
    case Bool(_) => TyBool
    case Eq(e1, e2) => 
      val (ctx1, ty1) = tyInfer(e1, ctx) 
      val (ctx2, ty2) = tyCheck(e2, ctx1, ty1) 
      if isEqType(ty1) {
        (ctx2, ty1)
      }
      else{sys.error("tyInfer: doesnt type check")}

    case Less(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyInt) 
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyInt) 
      (ctx2, TyBool)

    case IfThenElse(e1, e2, e3) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyBool) 
      val (ctx2, ty2) = tyInfer(e2, ctx1) 
      val (ctx3, ty3) = tyInfer(e3, ctx1) 
      val ty4 = tyJoin(ty2, ty3)
      val ctx4 = envIntersect(ctx2, ctx3)
      (ctx4, ty4)

    // Strings
    case Str(_) => TyString
    case Length(e1) =>
      val (ctx1, ty1) = tyCheck(e1, ctx, TyString)
      (ctx1, ty1)

    case Index(e1,e2) =>
      val (ctx1, ty1) = tyCheck(e1, ctx, TyString)
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyInt)
      (ctx2, TyString)
    
    case Concat(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyString)
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyString)
      (ctx2, TyString)

    case Print(e1, e2) => 
      val (ctx1, ty1) = tyCheck(e1, ctx, TyString)
      val (ctx2, ty2) = tyCheck(e2, ctx1, TyWorld)
      (ctx2, TyWorld)

      // Variables
    case Var(e1) =>
      val (ctx1, ty1) = tyInfer(e1, ctx)
      if tyIsRestricted(ty1){
        (ctx1 - e1, normTy(ty1))
        
      }
      if tyIsUnrestricted(ty1){
        (ctx1, normTy(ty1))

      }
      else{sys.error("tyInfer: doesnt type check")}

      case Unit => (ctx, TyUnit)

      case Pair(e1, e2) => 
        val (ctx1, ty1) = tyInfer(e1, ctx)
        val (ctx2, ty2) = tyInfer(e2, ctx1)
        (cxt2, TyPair(ty1, ty2))

      case PmPair(x, y, e1, e2) =>
        val (ctx1, typ) = tyInfer(e1, ctx)
        typ match {
          case TyPair(ty1, ty2) => 
            val ctx2 = ctx1 + (x -> ty1) + (y -> ty2)
            val (ctx3, ty3) = tyInfer(e2, ctx2)
            (ctx3, ty3)
          case _ => sys.error("tyInfer: doesnt type check")
        }

        //  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr

      case Lambda(x, tyx, e1) =>
        val ctx1 = ctx + (x -> tyx)
        val (ctx2, ty2) = tyInfer(e, ctx1)
        val ctx3 = ctx2 - x
        (ctx3, normTy(TyAFun(tyX, tyBody)))

//  case class Rec(f: Variable, x: Variable, ty1: Type, ty2: Type, e: Expr) extends Expr

      case Rec(f, x, ty1, ty2, e1) =>
        val ctx1 = ctx + (f -> TyBang(TyAFun(ty1, ty2))) + (x -> ty1)
        val (ctx2, ty3) = tyInfer(e1, ctx1)
        val ctx3 = ctx2 - f - x
        (ctx3, normTy(TyBang(TyAFun(ty1, ty3))))


      case Apply(e1, e2) =>
      val (ctx1, ty1) = tyInfer(e1, ctx)
      ty1 match {
      case TyAFun(argTy, retTy) =>
        val ctx2 = tyCheck(e2, ctx1, argTy)
        (ctx2, retTy)

      case TyBang(TyAFun(argTy, retTy)) =>
        val ctx2 = tyCheck(e2, ctx1, argTy)
        (ctx2, retTy)

      case _ =>
        sys.error("tyInfer: expected a function type in application")
       }

      case Left(e) =>
      val (ctx1, ty1) = tyInfer(e, ctx)
      (ctx1, TySum(ty1, TyNothing))

      case Right(e) =>
      val (ctx1, ty1) = tyInfer(e, ctx)
      (ctx1, TySum(TyNothing, ty1))

      //  case class Case(e: Expr, x: Variable, e1: Expr, y: Variable, e2: Expr) extends Expr
      case Case(e, x, e1, y, e2) =>
      val (ctx1, tys) = tyInfer(e, ctx)
      tys match{
        case TySum(ty1, ty2) => 
          val ctx2 = ctx1 + (x -> ty1)
          val (ctx3, ty1a) = tyInfer(e1, ctx2)
          val ctx4 = ctx1 + (y -> ty2)
          val (ctx5, ty2a) = tyInfer(e2, ctx4)
          val tyr = tyJoin(ty1a, ty2a)
          val ctxa = envIntersect(ctx3, ctx5) - x 
          val ctxb = ctxa - y
          (ctxb, tyr)
        case _ => sys.error("tyInfer: no match")
      }

      case Let(x, e1, e2) =>
       val (ctx1, ty1) = tyInfer(e1, ctx)

      if (envIsUnrestricted(ctx, ctx1)) {
        val ctx2 = ctx1 + (x -> TyBang(ty1))
        val (ctx3, ty2) = tyInfer(e2, ctx2)
        (ctx3 - x, ty2)

      } else if (envIsRestricted(ctx, ctx1)) {
        val ctx2 = ctx1 + (x -> ty1)
        val (ctx3, ty2) = tyInfer(e2, ctx2)
        (ctx3 - x, ty2)

      } else {
        sys.error("tyInfer: unexpected let case")
      }

      case Fst(e) =>
        val (ctx1, ty) = tyInfer(e, ctx)
        ty match {
          case TyPair(ty1, ty2) => (ctx1, ty1)
          case _ => sys.error("tyInfer: expected a pair type in fst")
        }

      case Snd(e) =>
        val (ctx1, ty) = tyInfer(e, ctx)
        ty match {
          case TyPair(ty1, ty2) => (ctx1, ty2)
          case _ => sys.error("tyInfer: expected a pair type in snd")
        }
        
      case LetFun(f, x, tyX, e1, e2) =>
        // Type-check the function body first under x
        val (ctx1, tyBody) = tyInfer(e1, ctx + (x -> tyX))

        if (envIsRestricted(ctx, ctx1)) {
          // Restricted function: bind f : τ1 ⊸ τ2
          val ctx2 = ctx1 - x + (f -> TyAFun(tyX, tyBody))
          val (ctx3, ty2) = tyInfer(e2, ctx2)
          (ctx3 - f, ty2)

        } else if (envIsUnrestricted(ctx, ctx1)) {
          // Unrestricted function: bind f : !(τ1 ⊸ τ2)
          val ctx2 = ctx1 - x + (f -> TyBang(TyAFun(tyX, tyBody)))
          val (ctx3, ty2) = tyInfer(e2, ctx2)
          (ctx3 - f, ty2)

        } else {
          sys.error("tyInfer: unexpected let fun case")
        }

      case LetRec(f, x, tyX, tyRet, e1, e2) =>
        // Recursive function: f is in scope in e1
        val ctx1 = ctx + (f -> TyBang(TyAFun(tyX, tyRet))) + (x -> tyX)
        val (ctx2, _) = tyInfer(e1, ctx1)

        if (envIsRestricted(ctx, ctx2)) {
          sys.error("tyInfer: recursive function body consumed restricted variables")
        } else {
          val ctx3 = ctx2 - x - f
          val ctx4 = ctx + (f -> TyBang(TyAFun(tyX, tyRet))) // bind f in body e2
          val (ctx5, ty2) = tyInfer(e2, ctx4)
          (ctx5 - f, ty2)
        }






    // END ANSWER
  }
}
