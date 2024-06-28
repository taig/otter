package io.taig.otter

trait PrimitiveOps[Self[_], Optional[_], Parent[_, _], Collection[_, _], Union[_, _], Writer[_]]
    extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
      ValidationOps[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]:
  extension [A](self: Self[A]) def tpe: Type[?]

object PrimitiveOps:
  trait Isomorphic[Self[_], Optional[_], Parent[_, _], Collection[_, _], Union[_, _], Writer[_]]
      extends PrimitiveOps[Self, Optional, Parent, Collection, Union, Writer],
        SchemaOps.Isomorphic[
          [_, a] =>> Self[a],
          [_, a] =>> Optional[a],
          Parent,
          Collection,
          Union
        ],
        ValidationOps.Isomorphic[
          [_, a] =>> Self[a],
          [a] =>> Constraint.Primitive[(Writer[a], a)],
          [a] =>> (Writer[a], a)
        ]

  trait Reader[Self[_], Optional[_], Parent[_, _], Collection[_, _], Union[_, _], Writer[_]]
      extends PrimitiveOps[Self, Optional, Parent, Collection, Union, Writer],
        SchemaOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
        ValidationOps.Reader[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]

  trait Writer[Self[_], Optional[_], Parent[_, _], Collection[_, _], Union[_, _], Writer[_]]
      extends PrimitiveOps[Self, Optional, Parent, Collection, Union, Writer],
        SchemaOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Parent, Collection, Union],
        ValidationOps.Writer[[_, a] =>> Self[a], [a] =>> Constraint.Primitive[(Writer[a], a)], [a] =>> (Writer[a], a)]
