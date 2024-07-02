package io.taig.otter

trait Container:
  type Schema[+A]
  type Collection[+A] <: Schema[A]
  type Dictionary[+A] <: Schema[A]
  type Dynamic[+A] <: Schema[A]
  type Enumeration[+A] <: Schema[A]
  type Primitive[+A] <: Schema[A]
  type Product[+A] <: Schema[A]
  type Record[+A] <: Schema[A]
  type Sum[+A] <: Schema[A]
  type Union[+A] <: Schema[A]
