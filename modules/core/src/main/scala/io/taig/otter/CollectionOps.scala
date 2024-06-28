package io.taig.otter

trait CollectionOps[Self[_, _], Union[_, _], Parent]
    extends TransformationOps[Self],
      SchemaOps[Self, Self, Self, Union]:
  extension [A, B](self: Self[A, B]) def schema: Parent

object CollectionOps:
  trait Isomorphic[Self[_, _], Union[_, _], Parent, Writer[_]]
      extends CollectionOps[Self, Union, Parent],
        TransformationOps.Isomorphic[Self, Writer, [_] =>> Constraint.Collection],
        SchemaOps.Isomorphic[Self, Self, Self, Union]

  trait Reader[Self[_, _], Union[_, _], Parent, Writer[_]]
      extends CollectionOps[Self, Union, Parent],
        TransformationOps.Reader[Self, Writer, [_] =>> Constraint.Collection],
        SchemaOps.Reader[Self, Self, Self, Union]

  trait Writer[Self[_, _], Union[_, _], Parent, Writer[_]]
      extends CollectionOps[Self, Union, Parent],
        TransformationOps.Writer[Self],
        SchemaOps.Writer[Self, Self, Self, Union]
