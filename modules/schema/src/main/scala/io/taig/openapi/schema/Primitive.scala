package io.taig.openapi.schema

import cats.data.Chain
import io.taig.openapi.{schema, OpenApi}
import io.taig.validation.{Constraint, Validation}

final case class Primitive[A](
    metadata: Primitive.Metadata[A],
    tpe: Type
) extends Value[A]:
  self =>

  override type Self[a] = Primitive[a]
  override type Codec = OpenApi.Primitive
  override type Metadata[a] = Primitive.Metadata[a]

  override def withMetadata(metadata: Primitive.Metadata[A]): Primitive[A] = copy(metadata = metadata)

  override def imap[B](f: A => B)(g: B => A): Primitive[B] = copy(metadata = metadata.map(f), tpe = tpe)

  override def ivalidate[B, C, D](validation: Validation[B, C, A, D])(g: D => A): Primitive[C] =
    copy(metadata = ???, tpe = tpe)

object Primitive:
  type Of[A <: Type] = A match
    case Type.BigDecimal.type => BigDecimal
    case Type.BigInt.type     => BigInt
    case Type.Boolean.type    => Boolean
    case Type.Double.type     => Double
    case Type.Float.type      => Float
    case Type.Int.type        => Int
    case Type.Long.type       => Long
    case Type.String.type     => String

  final case class Metadata[A](
      constraints: Chain[Constraint[OpenApi]],
      default: Option[A],
      description: Option[String],
      example: Option[A]
  ) extends Value.Metadata[A]:
    override type Self[a] = Primitive.Metadata[a]
    override def map[B](f: A => B): Primitive.Metadata[B] =
      Metadata(constraints, default.map(f), description, example.map(f))
    override def copy(default: Option[A], description: Option[String], example: Option[A]): Metadata[A] =
      Metadata(constraints, default, description, example)

  object Metadata:
    def empty[A]: Primitive.Metadata[A] = Metadata(Chain.empty, None, None, None)

  def empty[A <: Type & Singleton](tpe: A): Primitive[Of[A]] = new Primitive[Of[A]](Metadata.empty, tpe)
