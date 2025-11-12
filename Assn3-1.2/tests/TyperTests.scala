import scala.collection.immutable.ListMap

import Assign3.Syntax.Syntax._
import Assign3.Typer._
import Assign3.Interpreter._
import Assign3.Syntax.Syntax.SubstExpr.{subst}
  
val worldVar = "theWorld"

val tyCheck = Typer.tyCheck

val tyInfer = Typer.tyInfer

val parser = Interpreter.parser

val emptyEnv = ListMap[Variable, Type]()

def check(e: Expr, t: Type): Unit =
  val freshenedAst = subst(e,Var(worldVar),worldVar);
  tyCheck(Map(worldVar -> TyWorld), e, t)

def exp(s: String): Expr =
  val ast = parser.parseStr(s)
  //println(ast.toString)
  ast

def typ(s: String): Type = 
  val ast = parser.parseTyStr(s)
  //println(ast.toString)
  ast

// Test begin

val testTy0 = (
  "identity function",
  () => exp("\\x:int . x"),
  () => typ("int -o int")
)

val testTy1 = (
  "first",
  () => exp("\\x : int * bool. fst(x)"),
  () => typ("int * bool -o int")
)

val testTy2 = (
  "projection",
  () => exp("let fun f(x: int * int) = fst (x) + 42 in f (0,17)"),
  () => typ("int")
)

val testTy3 = (
  "inj",
  () => exp("let fun f(x: int * bool) = if (fst(x) == 0) then left(fst(x)) else right(snd(x)) in f (42,true)"),
  () => typ("int + bool")
)

val testTy4 = (
  "case split",
  () => exp("""
  let fun g(x: int + bool) = case x of {
      left x -> x
    | right b -> if b then 42 else 37
  } in g (right true)"""),
  () => typ("int")
)

val testTy5 = (
  "case split 2",
  () => exp("""
  let fun g(x : int + bool) = case x of {
      left x -> x
    | right b -> if b then 42 else 37
  } in g (left 1234)
  """),
  () => typ("int")
)

val testTy6 = (
  "repeat unrestricted function",
  () => exp("""
  let fun f(g: !(world -o world)) = \x: world.g(g(x)) in let fun id(x: world) = x in f(id)
  """),
  () => typ("world -o world")
)

val testTy7 = (
  "power",
  () => exp("""
  let power = rec power (input: int * int): int .
    let (x,n) = input in
    if (n == 0) then 1
    else x * power(x,n-1)
  in power(10,2)
  """),
  () => typ("int ")
)

val testTy8 = (
  "printing",
  () => exp("""
  let printHello = \x: world. print("Hello", x) in
  printHello(theWorld)
  """),
  () => typ("world")
)


def test(pack: (String, () => Expr, () => Type)) =
  val (s, e, ty) = pack
  print("Running <" ++ s ++ ">.")
  try {
    check(e(), ty())
    println(" Passed.")
  } catch {
    case exn:Throwable => {
      println(" Failed with exception: ")
      println(exn.toString) 
    }
  }

@main def runTests() = {
  test(testTy0)
  test(testTy1)
  test(testTy2)
  test(testTy3)
  test(testTy4)
  test(testTy5)
  test(testTy6)
  test(testTy7)
  test(testTy8)
}

  
