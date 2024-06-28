package io.taig.otter

trait CollectionOps[Self[_, _], Parent[_, _], Union[_, _], Any]
    extends TransformationOps[Self],
      SchemaOps[Self, Self, Parent, Self, Union]:
  extension [A, B](self: Self[A, B]) def schema: Any

object CollectionOps:
  trait Isomorphic[Self[_, _], Parent[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Parent, Any],
        TransformationOps.Isomorphic[Self, Writer, [_] =>> Constraint.Collection],
        SchemaOps.Isomorphic[Self, Self, Parent, Self, Union]

  trait Reader[Self[_, _], Parent[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Parent, Any],
        TransformationOps.Reader[Self, Writer, [_] =>> Constraint.Collection],
        SchemaOps.Reader[Self, Self, Parent, Self, Union]

  trait Writer[Self[_, _], Parent[_, _], Union[_, _], Writer[_], Any]
      extends CollectionOps[Self, Union, Parent, Any],
        TransformationOps.Writer[Self],
        SchemaOps.Writer[Self, Self, Parent, Self, Union]
