package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.Violations

sealed abstract class Branch[A](val name: String):
  final def :+[B](branch: Branch[B]): Coproduct[Either[A, B]] = toCoproduct :+ branch
  final def +:[B](branch: Branch[B]): Coproduct[Either[B, A]] = branch +: toCoproduct

  def toCoproduct: Coproduct[A] = Coproduct(this)

  final def decode(data: Data.Object, discriminator: Discriminator): Validated[Violations, Option[A]] =
    decode(name, data, discriminator)
  protected def decode(name: String, data: Data.Object, discriminator: Discriminator): Validated[Violations, Option[A]]
  final def encode(a: A, discriminator: Discriminator): Data.Object = encode(name, a, discriminator)
  protected def encode(name: String, a: A, discriminator: Discriminator): Data.Object

object Branch:
  def apply[A, B](name: A, key: Value[A], schema: Schema[B]): Branch[B] = new Branch[B](key.print(name).orEmpty):
    override def decode(
        name: String,
        data: Data.Object,
        discriminator: Discriminator
    ): Validated[Violations, Option[B]] = discriminator match
      case Discriminator.Nested(identifier, value) => ???
      case Discriminator.Merged(identifier)        => ???
      case Discriminator.Keyed                     => ???
    override def encode(name: String, b: B, discriminator: Discriminator): Data.Object = discriminator match
      case Discriminator.Nested(identifier, value) =>
        Data.Object.of(identifier -> Data.String(name), value -> schema.encode(b))
      case Discriminator.Merged(identifier) =>
        Data.Object.one(identifier, Data.String(name)) ++ schema.encode(b).asObject.getOrElse(Data.Object.Empty)
      case Discriminator.Keyed => Data.Object.one(name, schema.encode(b))
