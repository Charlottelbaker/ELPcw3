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
    case _ => sys.error("todo")
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
      case _ => sys.error("todo")
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
