package io.taig.otter

trait PrimitiveOps[Self[_], Optional[_], Collection[_, _], Union[_, _]]
    extends SchemaOps[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union]:
  extension [A](self: Self[A]) def tpe: Type[?]

object PrimitiveOps:
  trait Isomorphic[Self[_], Optional[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Collection, Union],
        SchemaOps.Isomporhic[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union]

  trait Reader[Self[_], Optional[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Collection, Union],
        SchemaOps.Reader[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union]

  trait Writer[Self[_], Optional[_], Collection[_, _], Union[_, _]]
      extends PrimitiveOps[Self, Optional, Collection, Union],
        SchemaOps.Writer[[_, a] =>> Self[a], [_, a] =>> Optional[a], Collection, Union]
