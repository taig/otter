package io.taig.otter

import cats.Contravariant

trait SchemaContravariant[Self[_, _], Optional[_, _], Collection[_, _], Union[_, _]]
    extends SchemaOps[Self, Optional, Collection, Union]:
  def contravariant[A]: Contravariant[Self[A, *]]
