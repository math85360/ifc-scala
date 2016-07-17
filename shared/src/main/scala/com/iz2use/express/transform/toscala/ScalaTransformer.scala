package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File

object ScalaTransformer {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(path: String, data: String): Unit =
    using(new FileWriter(path))(_.write(data))

  def appendToFile(path: String, data: String): Unit =
    using(new PrintWriter(new FileWriter(path, true)))(_.println(data))

  def transformSchema(schema: tree.Schema) = {
    val path = "target/generated"
    (new File(path)).mkdirs()
    schema.entities.foreach({ entity =>
      val content = transformEntity(entity)
      writeToFile(s"$path/${entity.name}.scala", content)
    })
  }
  def transformEntity(entity: tree.Entity) = {
    val inheritsFrom = if (entity.inheritsFrom.isEmpty) "" else " extends " + entity.inheritsFrom.mkString(" with ")
    val fields = entity.fields.map(transformField).mkString("\r\n", ",\r\n", "\r\n")
    val wheres = entity.wheres.map(transformWhere).mkString("\r\n")
    s"""class ${entity.name}($fields)$inheritsFrom {
$wheres
}"""
  }

  def transformField(field: tree.Field) = {
    s"""${field.name} : ${transformType(field.tpe)}"""
  }

  def transformWhere(where: tree.Where) = {
    s"""def ${where.name} = ${transformTree(where.expr)}"""
  }

  def transformOperator(op: tree.CompareOperator) = op match {
    case tree.NotEquals => "!="
    case tree.Equals => "=="
    case tree.LessThan => "<"
    case tree.LessThanOrEquals => "<="
    case tree.GreaterThan => ">"
    case tree.GreaterThanOrEquals => ">="
    case e => e
  }

  def transformOperator(op: tree.BooleanOperator) = op match {
    case tree.AND => "&&"
    case tree.OR => "||"
    //case tree.XOR => "XOR"
    case tree.IN => "IN"
    case e => e
  }

  def transformTree(_expr: tree.Tree): String = _expr match {
    case tree.Apply(fun, args) => args.map(transformTree).mkString(s"${transformTree(fun)}(", ", ", ")")
    //case a: tree.Select => a.toString()
    //case tree.Select(tree.Super(tree.This, _), name) => name
    case tree.Select(qlf, name) => s"${transformTree(qlf)}.$name"
    case tree.Group(expr) => s"(${transformTree(expr)})"
    case tree.Ident(name) => name
    case tree.Literal(tree.Constant(constant)) => constant match {
      case str: String => "\"" + str + "\""
      case e => e.toString()
    }
    //case tree.Super(tree.This, name) => s"super[]"
    case tree.Exists(expr) => s"${transformTree(expr)}.isDefined"
    case tree.Traverse(tree.Self, tree.Select(tree.Ident(a), rest)) => s"super[$a].$rest"
    case tree.Traverse(tree.Self, name) => s"super.${transformTree(name)}"
    //case tree.Traverse(ident, name) => s"${transformTree(ident)}\\${transformTree(name)}"
    case t: tree.Traverse => t.toString()
    case tree.BooleanOperation(tree.Literal(tree.Constant(lhs: String)), tree.IN, tree.TypeOf(rhs)) =>
      s"${transformTree(rhs)}.getClass.toString.toLowerCase.endsWith(" + "\"" + lhs.toLowerCase + "\"" + ")"
    //s"${transformTree(rhs)}.isInstanceOf($lhs)"
    case tree.BooleanOperation(lhs, tree.IN, rhs) => s"${transformTree(rhs)}.contains(${transformTree(lhs)}"
    case tree.CompareOperation(lhs, op, rhs) => s"${transformTree(lhs)} ${transformOperator(op)} ${transformTree(rhs)}"
    case tree.BooleanOperation(lhs, op, rhs) => s"${transformTree(lhs)} ${transformOperator(op)} ${transformTree(rhs)}"
    case tree.NotOperator(tree) => s"!${transformTree(tree)}"
    case tree.SizeOf(tree) => s"${transformTree(tree)}.length()"
    case tree.TypeOf(tree) => s"${transformTree(tree)}.getClass()"
    case tree.Query(name, query, cond) => s"${transformTree(query)}.exists({$name => ${transformTree(cond)}})"
    case expr => expr.toString()
  }

  def transformType(_tpe: tree.FieldType): String = _tpe match {
    case tree.OptionalField(tpe) => s"Option[${transformType(tpe)}]"
    case tree.ArrayType(tpe, dims, unique) => s"Array[${transformType(tpe)}]"
    case tree.SetType(tpe, dims, unique) => s"Set[${transformType(tpe)}]"
    case tree.ListType(tpe, dims, unique) => s"List[${transformType(tpe)}]"
    case tree.EnumerationType(list) => list.mkString("String /* Enumeration(", ", ", ") */")
    case tree.SelectType(list) => list.mkString("String /* Select(", ", ", ") */")
    case tree.StringType(length, fixed) => s"String /* $length, $fixed */"
    case tree.BinaryType(length, fixed) => s"String /*Binary $length, $fixed */"
    case tree.LogicalType | tree.BooleanType => s"Boolean"
    case tree.LongType => s"Long"
    case tree.IntegerType => s"Int"
    case tree.RealType => s"Double"
    case tree.NumberType => s"Double"
    case tree.UserDefinedType(name) => name
  }
}