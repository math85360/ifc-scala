package com.iz2use.express.parser

import com.iz2use.express.tree

trait Expr extends Base {
  import fastparse.noApi._
  import White._

  type Tree = Parser[tree.Tree]

  type TreeFn = Parser[tree.Tree => tree.Tree]

  @inline
  def toApply(args: Seq[tree.Tree]) = (expr: tree.Tree) => tree.Apply(expr, args)

  val arrayAccess: TreeFn = P(SBRO ~/ condition.rep(0, COMA) ~ SBRC).map(_.map(c => tree.ArithmeticOperation(c, tree.Difference, tree.Literal(tree.Constant(1L))))).map(toApply)

  val applyAccess: TreeFn = P(NBRO ~/ condition.rep(0, COMA) ~ NBRC).map(toApply)

  @inline
  def toSuper(args: tree.Ident) = (expr: tree.Tree) => tree.Super(expr, args)
  val superAccess: TreeFn = P(BACKSLASH ~/ ident).map(toSuper)

  val arrayDef: Tree = P(SBRO ~/ (condition ~ ((COLON ~/ condition).rep(1) | (COMA ~/ condition).rep(1)).?).? ~ SBRC).map({
    case None => tree.ArrayPrimitives(Seq.empty)
    case Some((head, rest)) => tree.ArrayPrimitives(rest.foldLeft(Seq(head))((acc, cur) => acc ++ cur))
  })

  val self: Tree = P(SELF.!).map({ case _ => tree.Self })

  val ident: Parser[tree.Ident] = P(name.!).map({
    case name => tree.Ident(name)
  })

  val superPart: Tree = P(self | ident)

  val superSelect: Tree = P(superPart ~ (BACKSLASH ~ ident).?).map({
    case (root, subPath) => subPath.foldLeft(root)((acc, cur) => tree.Super(acc, cur))
  })

  val accessMethods = P((applyAccess | arrayAccess | superAccess).rep(0))

  val applyDefaultPart: Tree = P(superPart ~ (accessMethods ~ (DOT ~/ superPart ~ accessMethods).rep(0)).?).map({
    case (fun, None) => fun
    case (fun, Some((argsApplyList, suffixes))) =>
      suffixes.foldLeft(argsApplyList.foldLeft(fun)((acc, cur) => cur(acc)))({
        case (acc2, (name, args)) => args.foldLeft[tree.Tree](tree.Select(acc2, name))((acc3, cur3) => cur3(acc3))
      })
  })

  /*val arraySelect: Tree = P(superSelect ~ arrayAccess.rep(0)).map({
    case (root, indexes) => indexes.foldLeft(root)((acc, cur) => cur._2(acc, cur._1))
  })

  val applySelect: Tree = P(arraySelect ~ applyAccess.rep(0)).map({
    case (root, applies) => applies.foldLeft(root)((acc, cur) => cur._2(acc, cur._1))
  })

  val selectSelect: Tree = P(applySelect ~ ("." ~ applySelect).rep(0)).map({
    case (root, traversers) => traversers.foldLeft(root)((acc, cur) => tree.Select(acc, cur))
  })

  val applyDefaultPart: Tree = P(selectSelect)*/

  val applyQueryPart: Tree = P(QUERY ~ NBRO ~/ name.! ~ LT_STAR ~/ condition ~ PIPE ~ condition ~ NBRC).map({
    case (name, query, cond) => tree.Query(name, query, cond)
  })

  val applySpecialPart: Tree = P((
    TYPEOF.!.map(_ => tree.TYPEOF) |
    SIZEOF.!.map(_ => tree.SIZEOF) |
    EXISTS.!.map(_ => tree.EXISTS) |
    NVL.!.map(_ => tree.NVL) |
    HIINDEX.!.map(_ => tree.HIINDEX) |
    ABS.!.map(_ => tree.ABS) |
    SQRT.!.map(_ => tree.SQRT) |
    BLENGTH.!.map(_ => tree.BLENGTH) |
    AREA.!.map(_ => tree.AREA) |
    USEDIN.!.map(_ => tree.USEDIN)) ~ NBRO ~/ condition.rep(0, COMA) ~ NBRC).map({
    case (func, argList) => tree.DefaultFunctionCall(func, argList)
  })

  val applyPart: Tree = P(applyQueryPart | applySpecialPart | applyDefaultPart)

  val apply = P(applyPart)
  /*val apply: Tree = P(applyPart ~ ("\\" ~/ applyPart).rep(0)).map({
    case (head, tail) => tail.foldLeft(head)((acc, cur) => tree.Traverse(acc, cur))
  })*/

  val smallestPart: Tree = P(primitives | apply | arrayDef)

  val evaluable: Tree = P(smallestPart)

  val between: Tree = P(CBRO ~/ condition ~ CBRC).map({ cnd =>
    cnd match {
      case tree.CompareOperation(tree.CompareOperation(lv, lo, v), ho, hv) =>
        tree.Between(lv, lo == tree.LessThanOrEquals, v, ho == tree.LessThanOrEquals, hv)
      case e => e
      //tree.Between(lv, il, v, ih, hv)
    }
  })

  val parens: Tree = P(NBRO ~/ condition ~ NBRC).map(cnd => tree.Group(cnd))

  val factor: Tree = P(parens | between | evaluable)

  val repeat: Tree = P(REPEAT ~/ name.! ~/ COLON_EQ ~/ evaluable ~/ TO ~/ evaluable ~/ EOS ~/ functionBlock ~/ END_REPEAT ~/ EOS).map({
    case (v, start, end, body) => tree.Repeat(tree.Raw(v), start, end, body)
  })

  val assign: Tree = P(apply ~ COLON_EQ ~/ condition ~ EOS).map({
    case (lhs, rhs) => tree.ValDef(lhs, tree.NoModifiers, tree.NoName, tree.EmptyTree, rhs)
  })

  val productOperation: Tree = P(factor ~ ((
    STAR.!.map(_ => tree.Multiply) |
    SLASH.!.map(_ => tree.Divide) |
    MOD.!.map(_ => tree.Mod)) ~/ factor).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.ArithmeticOperation(acc, op, cur)
    }
  })

  val term: Tree = P(MINUS.!.? ~ productOperation).map({
    case (arg, op) => arg.foldLeft(op) { (acc, cur) => tree.NegativeOperator(acc) }
  })

  val sumOperation: Tree = P(term ~ ((
    PLUS.!.map(_ => tree.Sum) |
    MINUS.!.map(_ => tree.Difference)) ~/ term).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.ArithmeticOperation(acc, op, cur)
    }
  })

  val comparatorOperation: Tree = P(sumOperation ~ (
    (
      COLON_EQ_COLON.!.map(_ => tree.InstanceEquals) |
      COLON_LT_GT_COLON.!.map(_ => tree.InstanceNotEquals) |
      LT_GT.!.map(_ => tree.NotEquals) |
      LT_EQ.!.map(_ => tree.LessThanOrEquals) |
      LT.!.map(_ => tree.LessThan) |
      EQ.!.map(_ => tree.Equals) |
      GT_EQ.!.map(_ => tree.GreaterThanOrEquals) |
      GT.!.map(_ => tree.GreaterThan)) ~/ sumOperation).rep(0)).map({
    case (root, lst) => lst.foldLeft(root) {
      case (acc, (op, cur)) => tree.CompareOperation(acc, op, cur)
    }
  })

  val unaryOperation: Tree = P(NOT.!.? ~ comparatorOperation).map({
    case (arg, expr) => arg.foldLeft(expr) { (acc, cur) => tree.NotOperator(acc) }
  })

  val binaryLogicalOperation: Tree = P(unaryOperation ~ ((
    XOR.!.map(_ => tree.Xor) |
    OR.!.map(_ => tree.Or) |
    AND.!.map(_ => tree.And) |
    IN.!.map(_ => tree.In) |
    PIPE_PIPE.!.map(_ => tree.OrElse)) ~/ unaryOperation).rep(0)).map({
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
    case expr => tree.Return(expr)
  })

  val escapeStmt: Tree = P(ESCAPE ~/ EOS).map({
    case _ => tree.Escape
  })

  val caseLabel = P(!OTHERWISE ~ (apply | primitives) ~ COLON ~/ lineBlock).map({
    case (pat, body) => tree.CaseDef(pat, body)
  })

  val caseBlock: Tree = P(CASE ~/ apply ~/ OF ~/ caseLabel.rep(1) ~/ (OTHERWISE ~/ COLON ~/ lineBlock).? ~/ END_CASE ~/ EOS).map({
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