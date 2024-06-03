package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.validation.Validation

object Plain extends Dsl:
  final override type AsSchema[+A] = A
  final override type AsCollection[+A] = A
  final override type AsPrimitive[+A] = A
  final override type AsTuple[+A] = A
  final override type AsUnion[+A] = A

  override protected inline def asPrimitive[A](a: A): AsPrimitive[A] = a
  override protected inline def asCollection[A](a: A): AsCollection[A] = a
  override protected inline def asTuple[A](a: A): AsTuple[A] = a

  // override given schemaOps: SchemaOps[Schema, Schema] with
  //   extension [A](self: Schema[A]) override def optional: Schema[Option[A]] = ???

  // // override given schemaInvariant: SchemaInvariant[Schema] = new SchemaInvariant[Schema]:
  // //   extension [A](self: Schema[A]) def validate[B](validation: Validation[A, ?, ?, B])(f: B => A): Schema[B] = ???

  // // override given schemaFunctor: SchemaFunctor[Schema.Reader] = new SchemaFunctor[Schema.Reader]:
  // //   extension [A](self: Schema.Reader[A])
  // //     def validate[B](validation: Validation[A, ?, ?, B]): Schema.Reader[B] = self.validate(validation)

  // override given schemaContravariant: SchemaContravariant[Schema.Writer] = new SchemaContravariant[Schema.Writer]:
  //   override def contramap[A, B](fa: Schema.Writer[A])(f: B => A): Schema.Writer[B] =
  // fa.transform(_.contramap(f))

  // given PrimitiveOps[Primitive, Primitive] = new PrimitiveOps[Primitive, Primitive]:
  //   extension [A](self: Primitive[A])
  //     override def tpe: Type[?] = self.extract.fa.extract.fa.tpe
  //     override def optional: Primitive[Option[A]] = self match
  //       case Base.Isomorphic.Root(schema)                => ???
  //       case Base.Isomorphic.Modify(self, validation, f) => Base.Isomorphic.Modify(self.optional, ???, ???)

  // given PrimitiveOps[Primitive.Required, Primitive] = new PrimitiveOps[Primitive.Required, Primitive]:
  //   extension [A](self: Primitive.Required[A])
  //     override def tpe: Type[?] = self.extract.fa.extract.fa.tpe
  //     override def optional: Primitive[Option[A]] = ???
