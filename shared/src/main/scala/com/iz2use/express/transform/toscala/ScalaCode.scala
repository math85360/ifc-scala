package com.iz2use.express.transform.toscala

trait ScalaCode {
  def scalaCode(implicit context: Context): String
}