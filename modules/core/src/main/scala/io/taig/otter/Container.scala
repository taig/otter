package io.taig.otter

trait Container:
  type Schema[+A]
  type Collection[+A] <: Schema[A]
  type Primitive[+A] <: Schema[A]
  type Tuple[+A] <: Schema[A]
  type Union[+A] <: Schema[A]
