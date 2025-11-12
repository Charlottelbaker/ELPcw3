package Assign3.Parser

import Assign3.Syntax.Syntax._
import scala.collection.immutable.ListMap
import scala.collection.immutable.Set
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/*======================================================================
  The rest of this file is support code, which you should not (and do not
  need to) change.
  ====================================================================== */

class Parser extends StandardTokenParsers with PackratParsers {

  type P[+A] = PackratParser[A]

  def parseStr(input: String): Expr = {
    phrase(expression)(new lexical.Scanner(input)) match {
      case Success(ast, _) => ast
      case e: NoSuccess => sys.error(e.msg)
    }
  }

  def parseTyStr(input: String): Type = {
    phrase(typ)(new lexical.Scanner(input)) match {
      case Success(ast, _) => ast
      case e: NoSuccess => sys.error(e.msg)
    }
  }

  def parse(input: String): Expr = {
    val source = scala.io.Source.fromFile(input)
    val lines = try source.mkString finally source.close()
    parseStr(lines)
  }

  lexical.reserved ++= List("if", "then", "else",
    "nothing", "unit", "int", "str", "bool", "true", "false", "concat",
    "world", "index", "length", "case", "of", "left", "right", "let", "in",  "print",
    "fst", "snd", "fun", "rec"
  )
  lexical.delimiters ++= List("=","*", "\\", "+", "-", "(", ")", "{", "}", "==", ":", ".", "^",
    "->", ",", "<", ">", "<=", ">=", "!", "|", "()", "-o"
  )

  lazy val expression: P[Expr] =
    simpleExpr

  lazy val lambda: P[Expr] =
    ("\\" ~> ident) ~ (":" ~> typ) ~ ("." ~> expression) ^^ {
      case arg~ty~body => Lambda(arg, ty, body)
    }

  lazy val rec: P[Expr] =
    ("rec" ~> ident ~ ("(" ~> ident ~ (":" ~> typ <~ ")")) ~ (":" ~> typ)) ~ ("." ~> expression) ^^ {
      case f~(x~ty1)~ty2~body => Rec(f, x, ty1, ty2, body)
    }

  lazy val ifExpr: P[Expr] =
    ("if" ~> expression) ~
      ("then" ~> expression) ~
      ("else" ~> expression) ^^ {
        case cond~e1~e2 => IfThenElse(cond,e1,e2)
      }

  lazy val let: P[Expr] =
    ("let" ~> ident) ~
      ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case x~e1~e2 => Let(x,e1,e2)
      }

  lazy val letFun: P[Expr] =
    ("let" ~ "fun") ~> ident ~ ("(" ~> ident ~ (":" ~> typ <~ ")")) ~
      ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case f~(x~ty)~e1~e2 => LetFun(f,x,ty,e1,e2)
      }

  lazy val letRec: P[Expr] =
    ("let" ~ "rec") ~> ident ~ ("(" ~> ident ~ (":" ~> typ <~ ")") ~ (":" ~> typ)) ~
      ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case f~(x~ty1~ty2)~e1~e2 => LetRec(f,x,ty1,ty2,e1,e2)
      }

  lazy val pmPair: P[Expr] =
    ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
      ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case x~y~e1~e2 => PmPair(x,y,e1,e2)
      }

  lazy val caseVariant: P[Expr] =
    ("case" ~> expression) ~ ("of" ~> "{" ~> leftCaseClause ~ "|" ~ rightCaseClause <~ "}") ^^ {
      case e~((x1,e1)~_~(x2,e2)) => Case(e, x1, e1, x2, e2)
    }
  
  lazy val leftCaseClause: P[(Variable, Expr)] =
    ("left"~ident ~ "->" ~ expression ^^ {
      case _~name~_~e => (name, e)
    }) |
    ("left"~ ("(" ~> ident <~ ")") ~ "->" ~ expression ^^ {
      case _~name~_~e => (name, e)
    })

  lazy val rightCaseClause: P[(Variable, Expr)] =
    ("right"~ident ~ "->" ~ expression ^^ {
      case _~name~_~e => (name, e)
    }) |
    ("right"~("(" ~> ident <~ ")") ~ "->" ~ expression ^^ {
      case _~name~_~e => (name, e)
    })

  lazy val typ: P[Type] =
    tyFunp | tyUFunp | tyPairp | tySump | simpleType

  lazy val tyFunp: P[Type] =
    (tyPairp | tySump | simpleType) ~ "-o" ~ typ ^^ {
      case t1~_~t2 => TyAFun(t1, t2)
    }

  lazy val tyUFunp: P[Type] =
    (tyPairp | tySump | simpleType) ~ "->" ~ typ ^^ {
      case t1~_~t2 => TyBang(TyAFun(t1, t2))
    }

  lazy val tyPairp: P[Type] =
    simpleType ~ "*" ~ (tyPairp | simpleType) ^^ {
      case t1~_~t2 => TyPair(t1,t2)
    }

  lazy val tySump: P[Type] =
    simpleType ~ "+" ~ (tySump | simpleType) ^^ {
      case t1~_~t2 => TySum(t1,t2)
    }

  lazy val tyBang: P[Type] =
    "!" ~> typ ^^ {
      case t => TyBang(t)
    }

  lazy val simpleType: P[Type] = (
    primitiveType | tyBang
  )

  lazy val primitiveType: P[Type] =
    "nothing" ^^^ TyNothing | "world" ^^^ TyWorld | "unit" ^^^ TyUnit | "bool" ^^^ TyBool | "int" ^^^ TyInt | "str" ^^^ TyString |  "("~>typ<~")"

  lazy val operations: P[Expr] =
    application |
    ("length" ~ "(") ~> expression <~ ")" ^^ (x => Length(x)) |
    ("concat"  ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
      case e1~e2 => Concat(e1,e2)
    } |
    ("index" ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
      case e1~e2 => Index(e1,e2)
    } |
    ("print"  ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
      case e1~e2 => Print(e1,e2)
    }

  lazy val arith: P[Expr] =
    comp

  lazy val prod: P[Expr] =
    prod ~ "*" ~ fact ^^ {
      case e1~_~e2 => Times(e1,e2)
    } | fact

  lazy val summation: P[Expr] =
    summation ~ "+" ~ prod ^^ {
      case e1~_~e2 => Plus(e1,e2)
    } | summation ~ "-" ~ prod ^^ {
      case e1~_~e2 => Minus(e1,e2)
    } | prod

  lazy val comp: P[Expr] =
    simpleExpr ~ "==" ~ summation ^^ {
      case e1~_~e2 => Eq(e1,e2)
    } | 
    simpleExpr ~ "<" ~ summation ^^ {
      case e1~_~e2 => Less(e1,e2)
    } | 
    summation

  lazy val application: P[Expr] =
    fact ~ fact ^^ {
      case e1~e2 => Apply(e1,e2)
    }

  lazy val simplerExpr: P[Expr] = (
    lambda |
    rec |
    pmPair |
    let |
    letFun |
    letRec |
    caseVariant |
    ifExpr |
    arith |
    fact
  )

  lazy val simpleExpr: P[Expr] = (
    simplerExpr
  )

  lazy val pairLit: P[Expr] =
    "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
      case t1~_~t2 => Pair(t1,t2)
    }

  lazy val left: P[Expr] =
    "left" ~> fact ^^ {
      case e => Left(e)
    }

  lazy val right: P[Expr] =
    "right" ~> fact ^^ {
      case e => Right(e)
    }

  lazy val first: P[Expr] =
    "fst" ~> fact ^^ {
      case e => Fst(e)
    }

  lazy val second: P[Expr] =
    "snd" ~> fact ^^ {
      case e => Snd(e)
    }

  lazy val fact: P[Expr] = (
    operations |
      pairLit |
      left |
      right |
      first |
      second |
      (ident ^^ {x => Var(x)}) |
      (numericLit ^^ {x => Num(x.toInt) }) |
      (stringLit ^^ {s => Str(s)}) |
      ("true" ^^^ Bool(true)) |
      ("false" ^^^ Bool(false)) |
      ("()" ^^^ Unit) |
      "("~>expression<~")"
  )

}
