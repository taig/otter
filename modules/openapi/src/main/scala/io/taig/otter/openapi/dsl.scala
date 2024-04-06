package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity
import io.taig.otter.Attribute
import io.taig.otter.openapi as OpenApi

object dsl extends Dsl:
  self =>

  override type Schema[A] = OpenApi.Schema[A]

  override type Value[A] = OpenApi.Value[A]

  override type Primitive[A] = OpenApi.Primitive[A]

  override object Primitive extends Primitives:
    override type Required[A] = OpenApi.Primitive.Required[A]

  override def primitive[A](tpe: Plain.Type[A]): Primitive.Required[A] = ???

  // override type Schema[A] = Annotation[Plain.Schema[A], Metadata[Identity]]
  // override type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive[Identity]]
  // override object Primitive extends Primitives:
  //   override type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]
  // override type Product[A] = Annotation[Plain.Tuple[A], Metadata.Product[Identity]]
  // override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
  //   Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive.Default)

  // extension [A, M](self: Annotation[Plain.Schema[A], M])
  //   def imap[B](f: A => B)(g: B => A): Annotation[Plain.Schema[B], M] = self.copy(self = self.self.imap(f)(g))

  // given [A]: Conversion[Primitive[A], Metadata.Primitive[Attribute[Primitive[A], *]]] =
  //   self => ???

object Playground:
  import dsl.*
  import dsl.given
