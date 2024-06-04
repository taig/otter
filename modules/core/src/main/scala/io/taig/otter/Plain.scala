package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait Plain extends Dsl:
  final override type AsSchema[+A] = A
  final override type AsCollection[+A] = A
  final override type AsPrimitive[+A] = A
  final override type AsTuple[+A] = A
  final override type AsUnion[+A] = A

  override protected inline def asPrimitive[A](a: A): AsPrimitive[A] = a
  override protected inline def asCollection[A](a: A): AsCollection[A] = a
  override protected inline def asTuple[A](a: A): AsTuple[A] = a

  override given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *], Schema.Of[A, *]] = ???

  override given schemaFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *], Schema.Reader.Of[A, *]] = ???

  override given primitiveInvariant: PrimitiveInvariant[Primitive, Primitive] with
    extension [A](self: Primitive[A])
      override def constraints: Chain[Base.validation.Constraint[?]] = self.constraints
      override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): Primitive[B] =
        self.ivalidate(validation)(f)
      override def optional: Primitive[Option[A]] = self.optional
      override def tpe: Type[?] = self.tpe

  override given primitiveRequiredInvariant: PrimitiveInvariant[Primitive.Required, Primitive] with
    extension [A](self: Primitive.Required[A])
      override def constraints: Chain[Constraint[?]] = self.constraints
      override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(
          f: B => A
      ): Primitive.Required[B] = self.ivalidate(validation)(f)
      override def optional: Primitive[Option[A]] = self.optional
      override def tpe: Type[?] = self.tpe

object Plain extends Plain
