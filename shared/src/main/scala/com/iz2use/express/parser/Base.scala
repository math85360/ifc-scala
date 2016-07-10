package com.iz2use.express.parser

import fastparse.Utils._
import fastparse.WhitespaceApi

trait Base extends Symbols {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace((" " | "\t" | "\r\n" | "\r" | "\n").rep(0))
  }

  import fastparse.noApi._
  import White._

  def group[A](parser: Parser[A]) = P("(" ~ parser ~ ")")

  val nameList = group(name.!.rep(1, ","))

  val expr = P((!EOS ~ AnyChar).rep(1))

}