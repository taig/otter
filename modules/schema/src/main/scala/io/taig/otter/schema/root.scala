package io.taig.otter.schema

type Value[A] = Schema.Value[A]
type Collection[A] = Schema.Collection[Schema, A]
object Collection:
  type Of[F[a] <: Schema[a], A] = Schema.Collection[F, A]
type Dictionary[A] = Schema.Dictionary[A]
type Coproduct[A] = Schema.Coproduct[A]
type Enumeration[A] = Schema.Enumeration[A]
type Primitive[A] = Schema.Primitive[A]
type Product[A] = Schema.Product[A]
type Record[A] = Schema.Record[A]
type AnyValue[A] = Schema.AnyValue[A]
