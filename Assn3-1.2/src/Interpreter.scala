package Assign3.Interpreter

import Assign3.Parser.Parser
import Assign3.Syntax.Syntax._
import Assign3.Typer.Typer
import scala.collection.immutable.Map

object Interpreter {

  import SubstExpr.{subst}

  // ======================================================================
  // Desugaring and Type Erasure
  // ======================================================================

    ////////////////////
    // EXERCISE 6     //
    ////////////////////
  def desugar(e: Expr): Expr = e match {
    // Value
    case v: Value =>
      sys.error("desugar: there shouldn't be any values here")
    // BEGIN ANSWER
    case Unit => Unit
    case Num(n) => Num(n)
    case Bool(b)        => Bool(b)
    case Str(s)         => Str(s)
    case Var(x)         => Var(x)
    case Loc(l)         => Loc(l)

    case Plus(e1,e2)    => Plus(desugar(e1), desugar(e2))
    case Minus(e1,e2)   => Minus(desugar(e1), desugar(e2))
    case Times(e1,e2)   => Times(desugar(e1), desugar(e2))

    case Eq(e1,e2)         => Eq(desugar(e1), desugar(e2))
    case Less(e1,e2)       => Less(desugar(e1), desugar(e2))
    case IfThenElse(c,t,f) => IfThenElse(desugar(c), desugar(t), desugar(f))

    case Length(e)      => Length(desugar(e))
    case Index(e1,e2)   => Index(desugar(e1), desugar(e2))
    case Concat(e1,e2)  => Concat(desugar(e1), desugar(e2))
    case Print(e1,e2)   => Print(desugar(e1), desugar(e2))

    case Let(x, e1, e2) => Let(x, desugar(e1), desugar(e2))

    case LetFun(f, x, ty, e1, e2) => Let(f, Lambda(x, ty, desugar(e1)), desugar(e2))

    case LetRec(f, x, ty1, ty2, e1, e2) => Let(f, Rec(f,x,ty1,ty2,desugar(e1)), desugar(e2))

    case Fst(e) =>
      val x = generator.genVar("x")
      val y = generator.genVar("y")
      PmPair(x, y, desugar(e), Var(x))
        
    case Snd(e) =>
      val x = generator.genVar("x")
      val y = generator.genVar("y")
      PmPair(x, y, desugar(e), Var(y))

    case Left(e)        => Left(desugar(e))
    case Right(e)       => Right(desugar(e))
    case Case(e, x, e1, y, e2) => Case(desugar(e), x, desugar(e1), y, desugar(e2))

    case Lambda(x, ty, e) => Lambda(x, ty, desugar(e))

    case Rec(f, x, ty1, ty2, body) => Rec(f, x, ty1, ty2, desugar(body))

    case Apply(e1, e2) => Apply(desugar(e1), desugar(e2))
     
  }

    // END ANSWER
  }


  // ======================================================================
  // Simple operations
  // ======================================================================

  object SimpleOp {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV (v1 + v2)
      case _ => sys.error("arguments to addition are non-numeric")

    def subtract(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV (v1 - v2)
      case _ => sys.error("arguments to subtraction are non-numeric")

    def multiply(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV (v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")

    def less(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => BoolV (v1 < v2)
      case _ => sys.error("arguments to < are not comparable")

    def length(v: Value): Value = v match
      case StringV(v1) => NumV(v1.length)
      case _ => sys.error("argument to length is not a string")

    def index(v1: Value, v2: Value): Value = (v1, v2) match
      case (StringV(v1), NumV(v2)) => StringV(v1.charAt(v2).toString)
      case _ => sys.error("arguments to index are not valid")

    def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (StringV(v1), StringV(v2)) => StringV(v1 ++ v2)
      case _ => sys.error("arguments to concat are not strings")

    def print(v1: Value, v2: Value): Value = (v1, v2) match
      case (StringV(v1), WorldV(v2)) => WorldV(v2 ++ v1)
      case _ => sys.error("arguments to print is not a string or a world")
  }


  // ======================================================================
  // Evaluation
  // ======================================================================

  type Store = Map[Variable, Expr]

  // ======================================================================
  // Equality Testing
  // ======================================================================
  
    ////////////////////
    // EXERCISE 7     //
    ////////////////////
    def eval(e : Expr, s : Store) : (Value, Store) = e match {
      // Value
      case v: Value => (v, s)
      case Unit => (UnitV, s)
      // BEGIN ANSWER
      case Num(n) => (NumV(n), s)
      case Bool(b) => (BoolV(b), s)
      case Str(s) => (StringV(s), s)
      case Lambda(x, ty, e) => (LambdaV(x, e), s)
      case Rec(f, x, ty1, ty2, e) => (RecV(f, x, e), s)

      case Plus(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.add(v1, v2), s2)

      case Minus(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.subtract(v1, v2), s2)

      case Times(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.multiply(v1, v2), s2)

      case Less(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.less(v1, v2), s2)

      case Eq(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (BoolV(v1 == v2), s2)
      
      case Length(e1) =>
        val (v1, s1) = eval(e1, s)
        (SimpleOp.length(v1), s1)

      case Index(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.index(v1, v2), s2)

      case Concat(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.concat(v1, v2), s2)

      case Print(e1, e2) =>
        val (v1, s1) = eval(e1, s)
        val (v2, s2) = eval(e2, s1)
        (SimpleOp.print(v1, v2), s2)

      case IfThenElse(c, t, f) =>
        val (vc, s1) = eval(c, s)
        vc match {
          case BoolV(true) => eval(t, s1)
          case BoolV(false) => eval(f, s1)
          case _ => sys.error("condition not a boolean")
        }
      
      case Loc(l) =>
        s.get(l) match {
          case Some(e) => 
            val (v1, s1) = eval(e, s)
            (v, s1 + (l -> v))
          case None => sys.error("not found in store")
        }

      case Let(x, e1, e2) =>
        val l = generator.genVar("loc")
        val s1 = s + (l -> e1)
        val e2Subst = subst(e2, Var(l), x)
        eval(e2Subst, s1)
      
      case Apply(e1, e2) =>
          val (vf, s1) = eval(e1, s)
          vf match {
              case LambdaV(x, body) =>
                val l = generator.genVar("l")
                val s2 = s1 + (l -> e2)
                val bodySub = subst(body, Loc(l), x)  
                eval(bodySub, s2)

              case RecV(f, x, e) =>
                val lx = generator.genVar("lx")      
                val lf = generator.genVar("lf")      
                val s2 = s1 + (lx -> e2) + (lf -> vf) 
                val bodySub = subst(subst(body, Loc(lx), x), Loc(lf), f) 
                eval(bodySub, s2)

              case _ => sys.error("Trying to apply a non-function")}

      case PmPair(x, y, e1, e2) =>
        val (vp, s1) = eval(e1, s)
        vp match {
          case PairV(ex, ey) =>
            val lx = generator.genVar("lx")
            val ly = generator.genVar("ly")
            val s2 = s1 + (lx -> ex) + (ly -> ey)
            val e2Sub = subst(subst(e2, Loc(lx), x), Loc(ly), y)
            eval(e2Sub, s2)
          case _ => sys.errors"Trying to pattern match a non-pair value")

      case Case(e, x, e1, y, e2) =>
        val (vsum, s1) = eval(e, s) 
        vsum match {
          case LeftV(vl) =>
            val lx = generator.genVar("lx")
            val s2 = s1 + (lx -> vl)              
            val e1Sub = subst(e1, Loc(lx), x)    
            eval(e1Sub, s2)                       

          case RightV(vr) =>
            val ly = generator.genVar("ly")      
            val s2 = s1 + (ly -> vr)            
            val e2Sub = subst(e2, Loc(ly), y)    
            eval(e2Sub, s2)

          case _ =>
            sys.error("Trying to pattern match a non-sum value")
        }
      
      // END ANSWER
    }

  /////////////////////////////////////////////////////////
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
  // THE REST OF THIS FILE SHOULD NOT NEED TO BE CHANGED //
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
  /////////////////////////////////////////////////////////
  
  // ======================================================================
  // Some simple programs
  // ======================================================================

  // The following examples illustrate how to embed Frog source code into
  // Scala using multi-line comments, and parse it using parser.parseStr.

  // Example 1: the swap function
  def example1: Expr = parser.parseStr("""
    let swap = \ x . (snd(x), fst(x)) in
    swap(42,17)
    """)

  val parser = new Parser

  // ======================================================================
  // Main
  // ======================================================================

  object Main {
    val worldVar = "theWorld"
    val worldVal = WorldV("")
    def typecheck(ast: Expr):Type =
      val freshenedAst = subst(ast,Var(worldVar),worldVar);
      println("Freshened AST: " + freshenedAst.toString);
      Typer.tyInfer(Map(worldVar -> TyWorld), freshenedAst)(1);

    def evaluate(ast: Expr): Value =
      eval(subst(ast,worldVal,worldVar), Map.empty)(0)

    def showResult(ast: Expr) = {
      println("AST:  " + ast.toString + "\n")

      try {
        print("Type Checking...");
        val ty = typecheck(ast);
        println("Done!");
        println("Type of Expression: " + ty.toString + "\n") ;
      } catch {
          case e:Throwable => println("Error: " + e)
      }
      try {
        println("Desugaring...");
        val core_ast = desugar(ast);
        println("Done!");
        println("Desugared AST: " + core_ast.toString + "\n") ;

        println("Evaluating...");
        println("Result: " + evaluate(core_ast))
      } catch {
        case e:Throwable => {
          println("Error: " + e)
          println("Evaluating raw AST...");
          println("Result: " + evaluate(ast))
        }
      }
    }

    def start(): Unit = {
      println("Welcome to Tidy! (V1.0, October 22, 2025)");
      println("Enter expressions to evaluate, :load <filename.tidy> to load a file, or :quit to quit.");
      println("This REPL can only read one line at a time, use :load to load larger expressions.");
      repl()
    }

    def repl(): Unit = {
      print("Tidy> ");
      val input = scala.io.StdIn.readLine();
      if(input == ":quit") {
        println("Goodbye!")
      }
      else if (input.startsWith(":load")) {
        try {
          val ast = parser.parse(input.substring(6));
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      } else {
        try {
          val ast = parser.parseStr(input);
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      Main.start()
    } else {
      try {
        print("Parsing...");
        val ast = parser.parse(args.head)
        println("Done!");
        Main.showResult(ast)
      } catch {
        case e:Throwable => println("Error: " + e)
      }
    }
  }

}
