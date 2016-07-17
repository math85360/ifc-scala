package com.iz2use.express.parser

import fastparse.Utils._
import com.iz2use.express.tree

trait Expr extends Base {
  import fastparse.noApi._
  import White._

  type Tree = Parser[tree.Tree]

  val arrayAccess = P("[" ~/ condition.rep(0, ",") ~ "]")

  val applySelect = P("(" ~/ condition.rep(0, ",") ~ ")")

  val arrayDef: Tree = P("[" ~/ (condition ~ ((":" ~/ condition).rep(1) | ("," ~/ condition).rep(1)).?).? ~/ "]").map({
    case x => tree.Raw(x.toString())
  })

  val self: Tree = P(SELF.!).map({ case _ => tree.Self })

  val ident: Tree = P(name.!).map({
    case name => tree.Ident(name)
  })

  val pathPart: Tree = P(self | ident)

  /*val path: Tree = P(pathPart ~ ("\\" ~ pathPart ~ ".").?).map({
    case (root, subPath) => subPath.foldLeft(root)((acc, cur) => tree.Path(acc, cur))
  })*/

  val accessMethods = P((applySelect | arrayAccess).rep(0))

  val applyDefaultPart: Tree = P(pathPart ~ accessMethods ~ ("." ~/ name.! ~ accessMethods).rep(0)).map({
    case (fun, argsApplyList, suffixes) =>
      suffixes.foldLeft(argsApplyList.foldLeft(fun)((acc, cur) => tree.Apply(acc, cur)))({
        case (acc, (name, args)) => args.foldLeft[tree.Tree](tree.Select(acc, name))((acc2, cur) => tree.Apply(acc2, cur))
      })
  })

  val applyQueryPart: Tree = P(QUERY ~ "(" ~/ name.! ~ "<*" ~/ condition ~ "|" ~ condition ~ ")").map({
    case (name, query, cond) => tree.Query(name, query, cond)
  })

  val applySpecialPart: Tree = P((TYPEOF.!.map(_ => tree.TypeOf) | SIZEOF.!.map(_ => tree.SizeOf) | EXISTS.!.map(_ => tree.Exists)) ~ "(" ~/ condition ~ ")").map({
    case (func, expr) => func(expr)
  })

  val applyPart: Tree = P(applyQueryPart | applySpecialPart | applyDefaultPart)

  val apply: Tree = P(applyPart ~ ("\\" ~/ applyPart).rep(0)).map({
    case (head, tail) => tail.foldLeft(head)((acc, cur) => tree.Traverse(acc, cur))
  })

  val smallestPart: Tree = P(primitives | apply | arrayDef)

  val evaluable: Tree = P(smallestPart)

  val between: Tree = P("{" ~/ condition ~ "}").map(lst => tree.Group(lst))

  val parens: Tree = P("(" ~/ condition ~ ")").map(cnd => tree.Group(cnd))

  val factor: Tree = P(parens | between | evaluable)

  val repeat: Tree = P(REPEAT ~/ name.! ~/ ":=" ~/ evaluable ~/ TO ~/ evaluable ~/ EOS ~/ functionBlock ~/ END_REPEAT ~/ EOS).map({
    case (v, start, end, body) => tree.Repeat(tree.Raw(v), start, end, body)
  })

  val assign: Tree = P(apply ~ ":=" ~/ condition ~ EOS).map({
    case (lhs, rhs) => tree.ValDef(lhs, tree.NoModifiers, tree.NoName, tree.EmptyTree, rhs)
  })

  val productOperation: Tree = P(factor ~ (("*" | "/" | MOD).! ~/ factor).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.Apply(tree.Select(acc, op), List(cur))
    }
  })

  val term: Tree = P("-".!.? ~ productOperation).map({
    case (arg, op) => arg.foldLeft(op) { (acc, cur) => tree.Apply(tree.Select(op, "unary_-"), List.empty) }
  })

  val sumOperation: Tree = P(term ~ (("+" | "-").! ~/ term).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.Apply(tree.Select(acc, op), List(cur))
    }
  })

  val comparatorOperation: Tree = P(sumOperation ~ (
    ((":" ~ ("=:" | "<>:")).!.map(tree.UnknownCompareOperator) |
      "<>".!.map(_ => tree.NotEquals) |
      "<=".!.map(_ => tree.LessThanOrEquals) |
      "<".!.map(_ => tree.LessThan) |
      "=".!.map(_ => tree.Equals) |
      ">=".!.map(_ => tree.GreaterThanOrEquals) |
      ">".!.map(_ => tree.GreaterThan)) ~/ sumOperation).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.CompareOperation(acc, op, cur)
    }
  })

  val unaryOperation: Tree = P(NOT.!.? ~ comparatorOperation).map({
    case (arg, expr) => arg.foldLeft(expr) { (acc, cur) => tree.NotOperator(acc) }
  })

  val binaryLogicalOperation: Tree = P(unaryOperation ~ ((
    XOR.!.map(_ => tree.XOR) |
    OR.!.map(_ => tree.OR) |
    AND.!.map(_ => tree.AND) |
    IN.!.map(_ => tree.IN) |
    ("||").!.map(tree.UnknownBooleanOperator)) ~/ unaryOperation).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.BooleanOperation(acc, op, cur)
    }
  })

  //val isolatedOperation = P( unaryLogicalOperation))

  val condition: Tree = P(binaryLogicalOperation)

  val ifBlock: Tree = P(IF ~/ condition ~/ THEN ~/ functionBlock ~/ (ELSE ~/ functionBlock).? ~ END_IF ~ EOS).map({
    case (cond, thenp, elsep) => tree.If(cond, thenp, elsep.getOrElse(emptyBlock))
    //case _ => tree.If(tree.EmptyTree, tree.EmptyTree, tree.EmptyTree)
  })

  val block: Tree = P(BEGIN ~/ functionBlock ~/ END ~ EOS)

  val returnStmt = P(RETURN ~/ condition ~ EOS).map({
    //case expr => tree.Return(expr)
    case _ => tree.Return(tree.EmptyTree)
  })

  val escapeStmt: Tree = P(ESCAPE ~/ EOS).map({
    case _ => tree.Escape
  })

  val caseLabel = P(!OTHERWISE ~ (apply | primitives) ~/ ":" ~ lineBlock).map({
    case (pat, body) => tree.CaseDef(pat, body)
  })

  val caseBlock: Tree = P(CASE ~/ apply ~/ OF ~/ caseLabel.rep(1) ~/ (OTHERWISE ~/ ":" ~/ lineBlock).? ~/ END_CASE ~/ EOS).map({
    case (selector, cases, otherwise) => tree.Match(selector, otherwise.map(tree.CaseDef(tree.EmptyTree, _)).foldLeft(cases)((lst, o) => lst :+ o))
  })

  val lineBlock: Tree = P((block | caseBlock | escapeStmt | ifBlock | repeat | returnStmt | assign))

  val functionBlock: Tree = P(lineBlock.rep(0)).map({
    case stats => tree.Block(stats.collect({
      case expr: tree.Tree => expr
      case raw => tree.Raw(raw.toString())
    }))
  })

}