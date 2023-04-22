package io.taig.openapi.schema

import cats.syntax.all.*

abstract class Value[A] extends Schema[A]:
  self =>
  override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }

  def default: Option[A]
  final def modifyDefault(f: Option[A] => Option[A]): Self[A] = copy(f(default), description, example, name)
  final def setDefault(default: Option[A]): Self[A] = self.modifyDefault(_ => default)
  final def withDefault(default: A): Self[A] = setDefault(default.some)
  final def withoutDefault(default: A): Self[A] = setDefault(none)

  def copy(
      default: Option[A],
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Self[A] { type Codec = self.Codec }

  final override def copy(
      description: Option[String],
      example: Option[A],
      name: Option[String]
  ): Self[A] { type Codec = self.Codec } = copy(default, description, example, name)
