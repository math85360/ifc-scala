package com.iz2use.express.transform.toscala

import com.iz2use.express.tree
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import com.iz2use.express.tree.OptionalField

object ScalaTransformer {
  val NL = "\r\n"
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(path: String, data: String): Unit =
    using(new FileWriter(path))(_.write(data))

  def appendToFile(path: String, data: String): Unit =
    using(new PrintWriter(new FileWriter(path, true)))(_.println(data))

  def transformSchema(implicit schema: tree.Schema) = {
    val path = s"../ifc4test/src/main/scala/express/${schema.name.toLowerCase}"
    (
      new File(path)).mkdirs()

    schema.entities.foreach({ entity =>
      val content = transformEntity(entity)(schema)
      writeToFile(s"$path/${entity.name}.scala", content)
    })

    schema.types.foreach({ tpe =>
      val content = transformDefinedType(tpe)(schema)
      writeToFile(s"$path/${tpe.name}.scala", content)
    })

    schema.functions.foreach({ tpe =>
      val content = transformFunction(tpe)(schema)
      writeToFile(s"$path/${tpe.name}.scala", content)
    })
  }

  @tailrec
  def allEntitiesInheritedBy(processing: Seq[tree.Entity], rest: Seq[tree.Entity] = Seq.empty)(implicit schema: tree.Schema): Seq[tree.Entity] = {
    //entity.inheritsFrom.foldLeft(
    processing match {
      case Nil =>
        rest
      case head :: tail =>
        val entities = (for {
          name <- head.inheritsFrom
          ent <- schema.entities.find(_.name == name)
        } yield ent)
        allEntitiesInheritedBy(tail ++ entities, head +: rest)
    } /*
    if (entity.inheritsFrom.isEmpty) {
      rest
    } else {
      
      entities.flatMap(allEntitiesInheritedBy(_, entities ++ rest))
    }*/
  }

  def transformEntity(entity: tree.Entity)(implicit schema: tree.Schema) = {
    val inverses = entity.inverses.map(transformInverse).mkString(NL)
    val derives = entity.derives.map(transformDerive).mkString(NL)
    val uniques = entity.derives.map(transformDerive).mkString(NL)
    val wheres = entity.wheres.map(transformWhere).mkString(NL)
    s"""package express.${schema.name.toLowerCase}
""" + (
      if (false && entity.isAbstract) {
        val fields = entity.fields.map(transformFieldInBody).mkString("(" + NL, NL + ", ", NL + ")")
        val inheritsFrom = if (entity.inheritsFrom.isEmpty) "" else " extends " + entity.inheritsFrom.mkString(" with ")
        s"""
trait ${entity.name}$inheritsFrom {
$fields
$inverses
$uniques
$wheres
}"""
      } else {
        val inheritedEntities = allEntitiesInheritedBy(Seq(entity))
        val inheritsFrom = if (entity.inheritsFrom.isEmpty) "" else " extends " + entity.inheritsFrom.map({ inh =>
          //if (inh.isAbstract) { inh } else {
          inheritedEntities.flatMap({ ent => if (ent == entity) Seq.empty else ent.fields }).map(_.name).mkString(s"$inh(", ", ", ")")
          //}
        }).mkString(" with ")
        val fields = inheritedEntities.flatMap(_.fields).map(transformInheritedField).mkString("(" + NL, NL + ", ", NL + ")")
        val prefix = (if (entity.isAbstract) "abstract " else "")
        s"""
${prefix}class ${entity.name}$fields$inheritsFrom {SELF =>
$inverses
$uniques
$wheres
}"""
      })
  }

  def transformDefinedType(entity: tree.Type)(implicit schema: tree.Schema) = {
    val wheres = entity.wheres.map(transformWhere).mkString(NL)
    s"""package express.${schema.name.toLowerCase}
""" +
      (entity.tpe match {
        case enum: tree.EnumerationType =>
          val elements = enum.list.map(i => s"""case object $i extends ${entity.name}""").mkString(NL)
          s"""
sealed trait ${entity.name} {
$wheres
}

object ${entity.name} {
$elements
}

"""

        case _ =>
          s"""
class ${entity.name}(SELF: ${transformType(entity.tpe)}) {
$wheres
}"""
      })
  }

  val defaultValueForType: PartialFunction[tree.FieldType, String] = {
    case tree.OptionalField(_) => "null"
    case tree.RealType => "0.0"
    case tree.NumberType | tree.LongType | tree.IntegerType => "0"
  }

  @tailrec
  def traverseType[A](_tpe: tree.FieldType)(implicit toKeep: PartialFunction[tree.FieldType, A]): Option[A] = {
    _tpe match {
      case tpe if toKeep.isDefinedAt(tpe) => Some(toKeep(tpe))
      case tree.OptionalField(tpe) => traverseType(tpe)
      case tree.ListType(tpe, _, _) => traverseType(tpe)
      case tree.SetType(tpe, _, _) => traverseType(tpe)
      case tree.ArrayType(tpe, _, _) => traverseType(tpe)
      case _ => None
    }
  }

  def transformFunction(entity: tree.Function)(implicit schema: tree.Schema) = {
    val args = entity.args.map(transformArgument).mkString("," + NL)
    val genericList = (entity.args.map(_.tpe) :+ entity.tpe).flatMap(traverseType(_)({
      case tree.GenericType(tpe) => tpe
    })).map({
      case tree.UserDefinedType(tpe) => tpe
    }).distinct
    val generics = (if (genericList.isEmpty) "" else genericList.mkString("[", ", ", "]"))
    val locals = entity.locals.map({ local =>
      val expr = local.expr.map(transformTree).fold(" = " + defaultValueForType.lift(local.tpe).getOrElse("null"))(" = " + _)
      s"""var ${local.name} : ${transformType(local.tpe)}$expr"""
    }).mkString(NL)
    s"""package express.${schema.name.toLowerCase}

object ${entity.name} {
def apply$generics($args) : ${transformType(entity.tpe)} = {
$locals
${transformTree(entity.body)}
}
}

"""
  }

  def transformArgument(arg: tree.Argument) = {
    s"""${arg.name} : ${transformType(arg.tpe)}"""
  }

  def transformFieldInBody(field: tree.Field) = {
    s"""val ${field.name} : ${transformType(field.tpe)} = _"""
  }

  def transformInheritedField(field: tree.Field) = {
    val defaultValue = defaultValueForType.lift(field.tpe).map(" = " + _).getOrElse("")
    s"""val ${field.name} : ${transformType(field.tpe)}$defaultValue"""
  }

  def transformInverse(field: tree.Inverse) = {
    s"""def ${field.name} : ${transformType(field.tpe)} = null // inverse from ${field.source}"""
  }

  def transformUnique(field: tree.Unique) = {
    s"""def unique_${field.name} = ${field.sources} // unique"""
  }

  def transformDerive(field: tree.Derive) = {
    s"""def ${transformTree(field.name)} : ${transformType(field.tpe)} = ${transformTree(field.expr)} // derive"""
  }

  def transformWhere(where: tree.Where) = {
    s"""def ${where.name} = ${transformTree(where.expr)} // where"""
  }

  def transformOperator(op: tree.Operator) = op match {
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

  def transformArgs(args: Seq[tree.Tree]): String = {
    args.map(transformTree).mkString(", ")
  }

  def transformTree(_expr: tree.Tree): String = {
    _expr match {
      case tree.Apply(fun, args) => args.map(transformTree).mkString(s"${transformTree(fun)}(", ", ", ")")
      case tree.Self => "SELF"
      case tree.Ident(name) => name
      case tree.Select(tree.Super(tree.Self, tree.Ident(_)), name) => transformTree(name)
      case tree.Select(tree.Self, name) => transformTree(name)
      case tree.Select(qlf, name) => s"${transformTree(qlf)}.${transformTree(name)}"
      case tree.Group(expr) => s"(${transformTree(expr)})"
      case tree.Ident(name) => name
      case tree.Literal(tree.Constant(constant)) => constant match {
        case str: String => "\"" + str + "\""
        case e => e.toString()
      }
      case tree.DefaultFunctionCall(tree.EXISTS, args) => s"${transformArgs(args)} != null"  // s"${transformArgs(args)}.isDefined"
      case tree.Super(tree.Self, tree.Ident(name)) => s"this" // case tree.Super(tree.Self, tree.Ident(name)) => s"super[$name]"
      case tree.Super(root, tree.Ident(name)) => s"${transformTree(root)}" //case tree.Super(root, tree.Ident(name)) => s"${transformTree(root)}.super[$name]"
      case tree.BooleanOperation(tree.Literal(tree.Constant(lhs: String)), tree.In, tree.DefaultFunctionCall(tree.TYPEOF, rhs)) =>
        s"${transformTree(rhs.head)}.getClass.toString.toLowerCase.endsWith(" + "\"" + lhs.toLowerCase + "\"" + ")"
      //s"${transformTree(rhs)}.isInstanceOf($lhs)"
      case tree.BooleanOperation(lhs, tree.In, rhs) => s"${transformTree(rhs)}.contains(${transformTree(lhs)})"
      case tree.ArithmeticOperation(tree.Literal(tree.Constant(lhs: Long)), tree.Difference, tree.Literal(tree.Constant(rhs: Long))) => (lhs - rhs).toString()
      case tree.BinaryOperation(lhs, op, rhs) => s"${transformTree(lhs)} ${transformOperator(op)} ${transformTree(rhs)}"
      case tree.NotOperator(tree) => s"!${transformTree(tree)}"
      case tree.NegativeOperator(tree) => s"-${transformTree(tree)}"
      case tree.DefaultFunctionCall(tree.SIZEOF, args) => s"${transformArgs(args)}.size"
      case tree.DefaultFunctionCall(tree.TYPEOF, args) => s"${transformArgs(args)}.getClass"
      case tree.DefaultFunctionCall(tree.ABS, args) => s"${transformTree(args.head)}.abs"
      case tree.DefaultFunctionCall(tree.NVL, args) => args.map(arg => s"Option(${transformTree(arg)})").reduce((a, b) => s"$a.orElse($b).get")
      case tree.DefaultFunctionCall(tree.HIINDEX, args) => s"(${transformTree(args.head)}.size - 1)"
      case tree.Between(lv, il, v, ih, hv) =>
        val c = transformTree(v)
        val l = (if (il) "=" else "")
        val h = (if (ih) "=" else "")
        s"((${transformTree(lv)} <$l $c) && ($c <$h ${transformTree(hv)}))"
      case tree.Query(name, query, cond) => s"${transformTree(query)}.filter({$name => ${transformTree(cond)}})"
      case tree.ArrayPrimitives(items) => items.map(transformTree).mkString("Array(", ", ", ")")
      case tree.If(cond, thenp, elsep) => s"""if(${transformTree(cond)}) {
${transformTree(thenp)}
}else{
${transformTree(elsep)}
}"""
      case tree.Block(items) => items.map(transformTree).mkString(NL)
      case tree.ValDef(lhs, _, _, _, rhs) => s"""${transformTree(lhs)} = ${transformTree(rhs)}"""
      case tree.Return(expr) => s"""return ${transformTree(expr)}"""
      case tree.EmptyTree => "null"
      case expr => s"${expr.toString()}"
    }
  }

  def transformType(_tpe: tree.FieldType): String = _tpe match {
    case tree.OptionalField(tpe) => transformType(tpe) // s"Option[${transformType(tpe)}]"
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
    case tree.GenericType(tpe) => transformType(tpe)
  }
}