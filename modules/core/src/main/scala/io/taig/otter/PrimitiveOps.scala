package io.taig.otter

trait PrimitiveOps[Self[_], Optional[_], Collection[_, _, _], Union[_, _, _], Plain[_]]:
  extension [A](self: Self[A]) def tpe: Type[?]
