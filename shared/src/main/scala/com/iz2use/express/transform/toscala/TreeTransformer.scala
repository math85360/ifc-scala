package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import Implicits._

object TreeTransformer {

  private def transformArgs(args: Seq[tree.Tree])(implicit context: Context): String = {
    args.map(apply).mkString(", ")
  }

  private def transformOperator(op: tree.Operator) = op match {
    case tree.NotEquals => "!="
    case tree.Equals => "=="
    case tree.LessThan => "<"
    case tree.LessThanOrEquals => "<="
    case tree.GreaterThan => ">"
    case tree.GreaterThanOrEquals => ">="
    case tree.And => "&&"
    case tree.Or => "||"
    case tree.Xor => "xor"
    case tree.In => "in"
    case tree.OrElse => "orElse"
    case tree.Sum => "+"
    case tree.Difference => "-"
    case tree.Multiply => "*"
    case tree.Divide => "/"
    case tree.Mod => "%"
    case tree.InstanceEquals => "=="
    case tree.InstanceNotEquals => "!="
    case e => e
  }

  def transformArgument(arg: tree.Argument)(implicit context: Context) = {
    s"""${arg.name} : ${arg.tpe.scalaCode}"""
  }

  def transformField(inBody: Boolean)(field: tree.Field)(implicit context: Context) = {
    val suffix = (if (inBody) { " = _" } else { field.tpe.defaultValue(" = " + _, "") })
    s"""var ${field.name} : ${field.tpe.scalaCode}$suffix"""
  }

  def transformInverse(field: tree.Inverse)(implicit context: Context) = {
    s"""def ${field.name} : ${field.tpe.scalaCode} = null // inverse from ${field.source}"""
  }

  def transformUnique(field: tree.Unique)(implicit context: Context) = {
    s"""def unique_${field.name} = ${field.sources} // unique"""
  }

  def transformDerive(field: tree.Derive)(implicit context: Context) = {
    s"""def ${TreeTransformer(field.name)} : ${field.tpe.scalaCode} = ${TreeTransformer(field.expr)} // derive"""
  }

  def transformWhere(where: tree.Where)(implicit context: Context) = {
    s"""def ${where.name} = ${TreeTransformer(where.expr)} // where"""
  }

  def apply(_expr: tree.Tree)(implicit context: Context): String = {
    _expr match {
      case tree.Apply(fun, args) => args.map(apply).mkString(s"${apply(fun)}(", ", ", ")")
      case tree.Self => "SELF"
      case tree.Ident(name) => context.getNormalizedNameFor(name)
      case tree.Select(tree.Super(tree.Self, tree.Ident(_)), name) => apply(name)
      case tree.Select(tree.Self, name) => apply(name)
      case tree.Select(qlf, name) =>
        // TODO .property must be a property from qlf
        s"${apply(qlf)}.${apply(name)}"
      case tree.Group(expr) => s"(${apply(expr)})"
      case tree.Literal(tree.Constant(constant)) => constant match {
        case str: String => "\"" + str + "\""
        case e => e.toString()
      }
      case tree.DefaultFunctionCall(tree.EXISTS, args) => s"(${transformArgs(args)} != null)" // s"${transformArgs(args)}.isDefined"
      case tree.Super(tree.Self, tree.Ident(name)) => s"this" // case tree.Super(tree.Self, tree.Ident(name)) => s"super[$name]"
      case tree.Super(root, tree.Ident(name)) => s"${apply(root)}" //case tree.Super(root, tree.Ident(name)) => s"${apply(root)}.super[$name]"
      case tree.BooleanOperation(tree.Literal(tree.Constant(lhs: String)), tree.In, tree.DefaultFunctionCall(tree.TYPEOF, rhs)) =>
        s"${apply(rhs.head)}.getClass.toString.toLowerCase.endsWith(" + "\"" + lhs.toLowerCase + "\"" + ")"
      //s"${apply(rhs)}.isInstanceOf($lhs)"
      case tree.BooleanOperation(lhs, tree.In, rhs) => s"${apply(rhs)}.contains(${apply(lhs)})"
      case tree.ArithmeticOperation(tree.Literal(tree.Constant(lhs: Long)), tree.Difference, tree.Literal(tree.Constant(rhs: Long))) => (lhs - rhs).toString()
      case tree.BinaryOperation(lhs, op, rhs) => s"${apply(lhs)} ${transformOperator(op)} ${apply(rhs)}"
      case tree.NotOperator(tree) => s"!${apply(tree)}"
      case tree.NegativeOperator(tree) => s"-${apply(tree)}"
      case tree.DefaultFunctionCall(tree.SIZEOF, args) => s"${transformArgs(args)}.size"
      case tree.DefaultFunctionCall(tree.TYPEOF, args) => s"${transformArgs(args)}.getClass"
      case tree.DefaultFunctionCall(tree.ABS, args) => s"${apply(args.head)}.abs"
      case tree.DefaultFunctionCall(tree.SQRT, args) => s"Math.sqrt(${apply(args.head)})"
      //case tree.DefaultFunctionCall(tree.NVL, args) => args.map(arg => s"Option(${apply(arg)})").reduce((a, b) => s"$a.orElse($b).get")
      case tree.DefaultFunctionCall(tree.HIINDEX, args) => s"(${apply(args.head)}.size - 1)"
      case tree.Between(lv, il, v, ih, hv) =>
        val c = apply(v)
        val l = (if (il) "=" else "")
        val h = (if (ih) "=" else "")
        s"((${apply(lv)} <$l $c) && ($c <$h ${apply(hv)}))"
      case tree.Query(name, query, cond) => s"${apply(query)}.filter({$name => " + apply(cond)(context.withLocals(name -> name)) + "})"
      case tree.ArrayPrimitives(items) => items match {
        case Nil => "empty"
        case lst => items.map(apply).mkString(" :: ")
      }
      case tree.If(cond, thenp, elsep) => s"""if(${apply(cond)}) {
${apply(thenp)}
}else{
${apply(elsep)}
}"""
      case tree.Block(items) => items.map(apply).mkString(context.NL)
      case tree.ValDef(lhs, _, _, _, rhs) => s"""${apply(lhs)} = ${apply(rhs)}"""
      case tree.Return(tree.EmptyTree) => /*"return " + context.collect({
        case tree.Function(_, _, tree.BooleanType, _, _) => ""
      }).getOrElse(apply(expr))*/
        "return null"
      case tree.Return(expr) => s"""return ${apply(expr)}"""
      case tree.EmptyTree => "null"
      case tree.DefaultFunctionCall(op, args) => "null /* $op : " + args.map(apply).mkString(" || ") + "*/"
      case tree.Repeat(tree.Ident(name), start, end, body) => s"for {$name <- ${TreeTransformer(start)} to ${TreeTransformer(end)} } {" + TreeTransformer(body)(context.withLocals(name -> name)) + "}"
      case expr => s"/*${expr.toString()}*/"
    }
  }
}