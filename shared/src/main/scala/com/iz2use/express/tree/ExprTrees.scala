package com.iz2use.express.tree

abstract sealed trait Name
case object NoName extends Name

abstract sealed trait Modifiers
case object NoModifiers extends Modifiers

abstract sealed trait Tree

case object Self extends Tree

case object EmptyTree extends Tree

case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree

case class ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) extends Tree

case class Return(expr: Tree) extends Tree

case class Constant(value: Any) extends Tree

case class Literal(value: Constant) extends Tree

case class Raw(raw: String) extends Tree

case class Block(stats: Seq[Tree]) extends Tree

case class Select(qualifier: Tree, name: String) extends Tree

case class Apply(fun: Tree, args: Seq[Tree]) extends Tree

case class CaseDef(pat: Tree, body: Tree) extends Tree

case class Match(selector: Tree, cases: Seq[Tree]) extends Tree

case class Repeat(name: Tree, start: Tree, end: Tree, body: Tree) extends Tree

case class Ident(name: String) extends Tree

case object Escape extends Tree

case class Traverse(fun: Tree, name: Tree) extends Tree

case class Group(expr: Tree) extends Tree

case class Between(lst: Seq[Tree]) extends Tree

abstract sealed trait Operator

abstract sealed trait BooleanOperator extends Operator

case object XOR extends BooleanOperator
case object OR extends BooleanOperator
case object AND extends BooleanOperator
case object IN extends BooleanOperator
case class UnknownBooleanOperator(op: String) extends BooleanOperator

abstract sealed trait CompareOperator extends Operator

case object NotEquals extends CompareOperator
case object Equals extends CompareOperator
case object LessThan extends CompareOperator
case object LessThanOrEquals extends CompareOperator
case object GreaterThan extends CompareOperator
case object GreaterThanOrEquals extends CompareOperator
case class UnknownCompareOperator(op: String) extends CompareOperator

case class CompareOperation(lhs: Tree, op: CompareOperator, rhs: Tree) extends Tree

case class BooleanOperation(lhs: Tree, op: BooleanOperator, rhs: Tree) extends Tree

case class NotOperator(tree:Tree) extends Tree with Operator

case class TypeOf(tree: Tree) extends Tree

case class SizeOf(tree: Tree) extends Tree

case class Exists(tree: Tree) extends Tree

case class Query(name: String, query: Tree, cond: Tree) extends Tree