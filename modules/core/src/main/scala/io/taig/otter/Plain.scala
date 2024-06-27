package io.taig.otter

import io.taig.otter as Base
import cats.Invariant

trait Plain extends Dsl:
  override object container extends Container:
    override type Schema[+A] = A
    override type Collection[+A] = A
    override type Primitive[+A] = A
    override type Tuple[+A] = A
    override type Union[+A] = A

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Base.Primitive.Required.Root(tpe)

  override given primitiveValidationInvariant: ValidationInvariant[Primitive, Constraint.Primitive] with
    extension [A](self: Primitive[A])
      override def ivalidate[B, C, D](validation: PrimitiveValidation[A, B, C, D])(g: D => A): Primitive[D] =
        self.ivalidate[B, C, D](validation)(g)

  override given unionInvariant: UnionInvariant[Union.Of, Union.Reader.Of, Union.Writer.Of, Schema.Of, Collection.Of] =
    new UnionInvariant[Union.Of, Union.Reader.Of, Union.Writer.Of, Schema.Of, Collection.Of] {
      override given invariant[A]: Invariant[Union.Of[A, *]] = ???

      extension [A, B](self: Union.Of[A, B])
        override def asReader: Union.Reader.Of[A, B] = self
        override def asWriter: Union.Writer.Of[A, B] = self
        override def collection: Collection.Of[A, B] = ???
        override def optional: Union.Of[A, Option[B]] = self.optional
        override def orElse[C, D](schema: Union.Of[C, D]): Union.Of[A | C, Either[B, D]] = self.orElse(schema)
        override def or[C, D](schema: Schema.Of[C, D]): Union.Of[A | schema.type, Either[B, D]] =
          orElse(Base.Union.Root(schema))
        override def union: Union.Of[A, B] = ???
    }

object Plain extends Plain
