package io.taig.otter.http

import cats.Eval
import cats.syntax.all.*
import io.taig.otter.schema.{Collection, Schema}
import org.typelevel.ci.CIString

// TODO default (?)
final case class Header[A](name: CIString, schema: Eval[Schema.Value[A] | Collection.Of[Schema.Value, A]]):
  def isOptional: Boolean = schema.value.isOptional
  def isCollection: Boolean = schema.value match
    case _: Collection.Of[Schema.Value, ?] => true
    case _                                 => false
