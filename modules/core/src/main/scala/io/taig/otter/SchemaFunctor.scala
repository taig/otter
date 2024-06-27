package io.taig.otter

import cats.Functor

trait SchemaFunctor[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
    extends SchemaOps[Self, Optional, Collection, Union]:
  def functor[A]: Functor[Self[A, *]]
