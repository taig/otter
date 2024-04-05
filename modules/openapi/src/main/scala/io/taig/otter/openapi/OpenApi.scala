package io.taig.otter.openapi

import io.taig.otter as Plain
import io.taig.otter.Dsl
import io.taig.otter.Type
import cats.Id as Identity

object OpenApi extends Dsl:
  self =>

  override type Schema[A] = Annotation[Plain.Schema[A], Metadata[Identity]]

  override type Value[A] = Annotation[Plain.Value[A], Metadata[Identity]]

  override type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive[Identity]]

  override object Primitive extends Primitives:
    override type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]
  override type Product[A] = Annotation[Plain.Product[A], Metadata.Product[Identity]]
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = ???
