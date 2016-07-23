package com.iz2use.express.transform.toscala

import com.iz2use.express.tree

object Implicits {
  implicit def transformedFieldType(tpe: tree.FieldType): FieldTypeTransformer = FieldTypeTransformer(tpe)
  implicit def transformedEntity(tpe: tree.Entity): EntityTransformer = EntityTransformer(tpe)
  implicit def transformedFunction(tpe: tree.Function) : FunctionTransformer = FunctionTransformer(tpe)
  implicit def transformedRule(tpe: tree.Rule) : RuleTransformer = RuleTransformer(tpe)
  implicit def transformedType(tpe: tree.Type): TypeTransformer = TypeTransformer(tpe)
  implicit def transformedUserDefinedType(tpe: tree.UserDefinedType) : UserDefinedTypeTransformer = UserDefinedTypeTransformer(tpe)
}