package io.taig.otter

final case class Json[A](
    self: Collection[Json, A] | Constant[Json, A] | Dictionary[Value, Json, A] | Enumeration[Primitive, A] |
      Optional[Json, A] | Primitive[A] | Record[Json, A] | Tuple[Json, A] | Union[Json, A]
)
