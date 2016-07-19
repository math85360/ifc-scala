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

case class Select(qualifier: Tree, selection: Tree) extends Tree

case class Apply(fun: Tree, args: Seq[Tree]) extends Tree

case class CaseDef(pat: Tree, body: Tree) extends Tree

case class Match(selector: Tree, cases: Seq[Tree]) extends Tree

case class Repeat(name: Tree, start: Tree, end: Tree, body: Tree) extends Tree

case class Ident(name: String) extends Tree

case object Escape extends Tree

//case class Traverse(fun: Tree, name: Tree) extends Tree

case class Super(ident: Tree, name: Ident) extends Tree

case class Group(expr: Tree) extends Tree

case class Between(low: Tree, includeLow: Boolean, value: Tree, includeHigh: Boolean, high: Tree) extends Tree

abstract sealed trait Operator

abstract sealed trait ArithmeticOperator extends Operator

case object Sum extends ArithmeticOperator
case object Difference extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator
case object Mod extends ArithmeticOperator

abstract sealed trait BooleanOperator extends Operator

case object Xor extends BooleanOperator
case object Or extends BooleanOperator
case object OrElse extends BooleanOperator
case object And extends BooleanOperator
case object In extends BooleanOperator
case class UnknownBooleanOperator(op: String) extends BooleanOperator

abstract sealed trait CompareOperator extends Operator

case object NotEquals extends CompareOperator
case object Equals extends CompareOperator
case object LessThan extends CompareOperator
case object LessThanOrEquals extends CompareOperator
case object GreaterThan extends CompareOperator
case object GreaterThanOrEquals extends CompareOperator
case object InstanceEquals extends CompareOperator
case object InstanceNotEquals extends CompareOperator
case class UnknownCompareOperator(op: String) extends CompareOperator

case class CompareOperation(override val lhs: Tree, override val op: CompareOperator, override val rhs: Tree) extends BinaryOperation(lhs, op, rhs)

case class BooleanOperation(override val lhs: Tree, override val op: BooleanOperator, override val rhs: Tree) extends BinaryOperation(lhs, op, rhs)

case class ArithmeticOperation(override val lhs: Tree, override val op: ArithmeticOperator, override val rhs: Tree) extends BinaryOperation(lhs, op, rhs)

case class NotOperator(tree: Tree) extends Tree with Operator

case class NegativeOperator(tree: Tree) extends Tree with Operator

case class DefaultFunctionCall(fun: DefaultFunction, args: Seq[Tree]) extends Tree

sealed abstract trait DefaultFunction

case object TYPEOF extends DefaultFunction
case object SIZEOF extends DefaultFunction
case object EXISTS extends DefaultFunction
case object NVL extends DefaultFunction
case object ABS extends DefaultFunction
case object HIINDEX extends DefaultFunction

case class Query(name: String, query: Tree, cond: Tree) extends Tree

case class ArrayPrimitives(items: Seq[Tree]) extends Tree

abstract class BinaryOperation[Op <: Operator](val lhs: Tree, val op: Op, val rhs: Tree) extends Tree {

}
object BinaryOperation {
  def unapply(tree: Tree): Option[(Tree, Operator, Tree)] = {
    tree match {
      case bin: BinaryOperation[_] => Some((bin.lhs, bin.op, bin.rhs))
      case _ => None
    }
  }
}