package io.taig.otter.http.header

import cats.Eq
import cats.Show
import cats.syntax.all.*
import org.typelevel.ci.CIString

opaque type Parameters = List[Parameter]

object Parameters:
  val Empty: Parameters = Nil

  extension (self: Parameters)
    inline def toList: List[Parameter] = self
    def get(key: CIString): List[String] = toList.collect { case Parameter(`key`, value) => value }

  def apply(values: List[Parameter]): Parameters = values

  def of(parameters: (CIString, String)*): Parameters = parameters.toList.map(Parameter.apply)

  given (using eq: Eq[List[Parameter]]): Eq[Parameters] = eq

  given Show[Parameters] = _.toList.map(parameter => show"; $parameter").mkString
