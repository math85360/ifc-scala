package com.iz2use.express.parser

import com.iz2use.express.tree
import fastparse.Utils._

object ExpressParser extends Symbols with FieldType with Base with Expr {
  import fastparse.noApi._
  import White._

  //val space = P(" " | "\t" | "\r\n" | "\r" | "\n")

  val field: Parser[tree.Field] = P(name.! ~ ":" ~/ OPTIONAL.!.? ~/ fieldType ~/ EOS).map({
    case (name, opt, tpe) => tree.Field(name, opt.foldLeft(tpe)((acc, cur) => tree.OptionalField(acc)))
  })

  val fields = P(field.rep(0))

  val deriveField = P((SELF ~ "\\").? ~ name.!.rep(1, ".") ~ ":" ~/ fieldType ~/ ":=" ~/ expr.! ~/ EOS)

  val whereField: Parser[tree.Where] = P(name.! ~ ":" ~/ condition ~/ EOS).map({
    case (name, expr) => tree.Where(name, expr)
  })

  val derives = P(DERIVE ~/ deriveField.rep(1))

  val inverseField = P(name.! ~ ":" ~/ fieldType ~/ FOR ~/ name.! ~/ EOS)

  val inverses = P(INVERSE ~/ inverseField.rep(1))

  val wheres = P(WHERE ~/ whereField.rep(1))

  val uniqueField = P(name.! ~ ":" ~/ name.!.rep(1, ",") ~/ EOS)

  val uniques = P(UNIQUE ~/ uniqueField.rep(1))

  val abstractOf = P(ABSTRACT.? ~ SUPERTYPE ~/ OF ~/ group(ONEOF ~/ nameList))

  val subtypeOf = P(SUBTYPE ~/ OF ~/ nameList)

  val entityDef = P(ENTITY ~/ name.! ~/ abstractOf.? ~/ subtypeOf.? ~/ EOS ~/ fields ~/ derives.? ~/ inverses.? ~/ uniques.? ~/ wheres.? ~/ END_ENTITY ~/ EOS).map({
    case (name, abstracts, subtypes, fields, derives, inverses, unqiues, wheres) => tree.Entity(name, subtypes.getOrElse(Seq.empty), fields, wheres.getOrElse(Seq.empty))
  })

  val typeDef = P(TYPE ~/ name.! ~/ "=" ~/ fieldType ~/ EOS ~/ wheres.? ~/ END_TYPE ~/ EOS)

  val localDef = P(name.rep(1, ",") ~ ":" ~/ fieldType ~/ (":=" ~/ expr.!).? ~/ EOS)

  val localDefs = P(LOCAL ~/ localDef.rep(1) ~/ END_LOCAL ~/ EOS)

  val functionDef = P(FUNCTION ~/ name.!.map({ x => println(x); x }) ~/ group((name.!.rep(1, ",") ~/ (":" ~/ fieldType).rep(1)).rep(1, EOS)) ~/ (":" ~/ fieldType).rep(1) ~/ EOS ~/ localDefs.? ~/ functionBlock ~/ END_FUNCTION ~/ EOS)

  val ruleDef = P(RULE ~/ name.! ~/ FOR ~/ nameList ~/ EOS ~/ localDefs.? ~ functionBlock ~ wheres.? ~/ (!END_RULE ~ AnyChar).rep(0) ~/ END_RULE ~/ EOS)

  val element = P(entityDef | typeDef | functionDef | ruleDef)

  val schemaBody = P(element.rep(0))

  val schemaDef = P(SCHEMA ~/ name.! ~/ EOS ~/ schemaBody ~/ END_SCHEMA ~ EOS).map({
    case (name, body) => tree.Schema(name,
      entities = body.collect({ case e: tree.Entity => e }))
  })

  val file = P(header ~ schemaDef).map({
    case (header, schema) => schema
  })
}
