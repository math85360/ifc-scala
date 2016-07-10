package com.iz2use.express.tree

abstract sealed trait Name
case object NoName extends Name

abstract sealed trait Modifiers
case object NoModifiers extends Modifiers

abstract sealed trait Tree

case object EmptyTree extends Tree

case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree

case class ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) extends Tree

case class Return(expr: Tree) extends Tree

case class Constant(value: Any) extends Tree

case class Literal(value: Constant) extends Tree

case class Raw(raw: String) extends Tree

case class Block(stats: List[Tree]) extends Tree

case class Select(qualifier: Tree, name: String) extends Tree

case class Apply(fun: Tree, args: List[Tree]) extends Tree

case class CaseDef(pat: Tree, body: Tree) extends Tree

case class Match(selector: Tree, cases: List[Tree]) extends Tree

case class Repeat(name: Tree, start: Tree, end: Tree, body: Tree) extends Tree

case class Ident(name: String) extends Tree

case object Escape extends Tree

case class Child(fun: Tree, name: Tree) extends Tree