package io.taig.otter

trait PrimitiveInvariant[F[+_], Self[_], Reader[_], Writer[_], Optional[_], Collection[_, _], Union[_, _]]
    extends SchemaInvariant[
      [_, a] =>> Self[a],
      [_, a] =>> Reader[a],
      [_, a] =>> Writer[a],
      [_, a] =>> Optional[a],
      Collection,
      Union
    ]:
  override given invariant[A]: ValidationInvariant[F, Self, Constraint.Primitive]
