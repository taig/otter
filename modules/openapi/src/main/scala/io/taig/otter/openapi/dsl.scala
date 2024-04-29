package io.taig.otter.openapi

import io.taig.otter as Plain
import cats.Id as Identity
import io.taig.otter.openapi as OpenApi
import io.taig.otter.Type
import io.taig.otter.Dsl
import io.taig.otter.Operation
import io.taig.otter.validation.Validation

final case class Annotation[+S, +M](self: S, metadata: M)

object dsl extends Dsl:
  self =>

  override type Schema[A] = Annotation[Plain.Schema[A], Metadata[Identity]]

  override type Value[A] = Annotation[Plain.Value[A], Metadata.Value[Identity]]

  override type Collection[A] = Annotation[Plain.Collection[?, A], Metadata.Collection[Identity]]

  override object Collection extends Collections {}

  override type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive[Identity]]

  override object Primitive extends Primitives:
    type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]

  override type Tuple[A] = Annotation[Plain.Tuple[?, A], Metadata[Identity]]

  override object Tuple extends Tuples:
    override type Of[+S, A] = Annotation[Plain.Tuple[S, A], Metadata.Tuple[Identity]]

  override def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    Annotation(Plain.Primitive.Required.Root(tpe), Metadata.Primitive.Default)

  override def toOperation[A](
      self: Annotation[Plain.Schema[A], Metadata[Identity]]
  ): Operation[Schema, Schema, Schema, Tuple.Of, A] = ???

  override def toOperationPrimitiveRequired[A](
      self: Annotation[Plain.Primitive.Required[A], Metadata.Primitive[Identity]]
  ): Operation.Primitive[Primitive.Required, Primitive, Schema, Tuple.Of, A] = new Operation.Primitive:
    export self.self.tpe
    override def asSelf: Primitive.Required[A] = self
    override def ivalidate[B, C](constraint: Schema[B])(validation: Validation[A, B, C])(
        g: C => A
    ): Primitive.Required[C] = self.copy(self = self.self.ivalidate(constraint.self)(validation)(g))
    override def optional: Primitive[Option[A]] = self.copy(self = self.self.optional)
    override def toTuple: Tuple.Of[Primitive.Required[A], A] =
      Annotation(self = Plain.Tuple.One(self), Metadata.Tuple.Default)

object Playground:
  import dsl.*
  import dsl.given

  val x: Tuple[String] = string.toTuple
