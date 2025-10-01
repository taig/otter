package io.taig.otter.operation

import cats.data.Chain
import io.taig.validation.Validation
import io.taig.otter.Annotation
import io.taig.otter.Primitive
import io.taig.otter.Constraint

trait StringSchemaInvariant[Self[_], +Constraint <: Constraint.Primitive] extends SchemaInvariant[Self]:
  self =>

  def string(validation: Validation[Constraint.Primitive.Text, String]): Self[String]

  def parser[A](
      name: String,
      decode: String => Either[String, A],
      encode: A => String
  ): Self[A]

  extension [A](self: Self[A]) def constraints: Chain[Constraint]

  override def imapK[G[_]](fK: [A] => Self[A] => G[A])(
      gK: [A] => G[A] => Self[A]
  ): StringSchemaInvariant[G, Constraint] = new StringSchemaInvariant[G, Constraint]:
    override def string(validation: Validation[Constraint.Primitive.Text, String]): G[String] =
      fK(self.string(validation))

    override def parser[A](name: String, decode: String => Either[String, A], encode: A => String): G[A] =
      fK(self.parser(name, decode, encode))

    extension [A](ga: G[A])
      override def constraints: Chain[Constraint] = self.constraints(gK(ga))
      override def imap[B](f: A => B)(g: B => A): G[B] = fK(self.imap(gK(ga))(f)(g))

object StringSchemaInvariant:
  inline def apply[Self[_], Constraint <: Constraint.Primitive](using
      invariant: StringSchemaInvariant[Self, Constraint]
  ): StringSchemaInvariant[Self, Constraint] = invariant

  given schema: StringSchemaInvariant[[a] =>> Annotation[Primitive.String[a]], Constraint.Primitive.Text] with
    override def string(
        validation: Validation[Constraint.Primitive.Text, String]
    ): Annotation[Primitive.String[String]] = Annotation(Primitive.String.Root(validation))

    override def parser[A](
        name: String,
        decode: String => Either[String, A],
        encode: A => String
    ): Annotation[Primitive.String[A]] = Annotation(Primitive.String.Parser(name, decode, encode))

    extension [A](self: Annotation[Primitive.String[A]])
      override def constraints: Chain[Constraint.Primitive.Text] = self.self.constraints
      override def imap[B](f: A => B)(g: B => A): Annotation[Primitive.String[B]] = self.map(_.imap(f)(g))
