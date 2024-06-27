package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.Constraint
import io.taig.otter.UnionInvariant
import io.taig.otter.Type

trait Dsl extends Base.Dsl:
  override object container extends Base.Container:
    override type Schema[+A] = Annotation[Metadata, A]
    override type Collection[+A] = Annotation[Metadata, A]
    override type Primitive[+A] = Annotation[Metadata.Primitive, A]
    override type Tuple[+A] = Annotation[Metadata, A]
    override type Union[+A] = Annotation[Metadata, A]

object Dsl extends Dsl {
  override def primitive[A](tpe: Type[A]): Primitive.Required[A] = Annotation(
    metadata = Metadata.Primitive(name = None),
    self = Base.Primitive.Required.Root(tpe)
  )

  override given primitiveValidationInvariant: ValidationInvariant[Primitive, Constraint.Primitive] = ???

  override given unionInvariant: UnionInvariant[Union.Of, Union.Reader.Of, Union.Writer.Of, Schema.Of, Collection.Of] =
    ???

}
