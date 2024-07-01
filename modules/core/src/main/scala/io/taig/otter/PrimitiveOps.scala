package io.taig.otter

trait PrimitiveOps[Self[_], Optional[_], Collection[_, _], Union[_, _], Plain[_]]
    extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union, Plain]:
  extension [A](self: Self[A]) def tpe: Type[?]

object PrimitiveOps:
  trait Isomorphic[Self[_], Optional[_], Collection[_, _], Union[_, _], Plain[a] <: Primitive[a]]
      extends PrimitiveOps[Self, Optional, Collection, Union, Plain],
        SchemaOps.Isomorphic[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union, Plain]

  trait Reader[Self[_], Optional[_], Collection[_, _], Union[_, _], Plain[a] <: Primitive.Reader[a]]
      extends PrimitiveOps[Self, Optional, Collection, Union, Plain],
        SchemaOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union, Plain]

  trait Writer[Self[_], Optional[_], Collection[_, _], Union[_, _], Plain[a] <: Primitive.Writer[a]]
      extends PrimitiveOps[Self, Optional, Collection, Union, Plain],
        SchemaOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union, Plain]
