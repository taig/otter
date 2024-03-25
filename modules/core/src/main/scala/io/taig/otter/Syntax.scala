package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.Key
import io.taig.hmap.HMap
import scala.annotation.targetName

trait Syntax[C <: Context] extends Types[C]:
  self =>

  extension [S <: Plain.Schema[A], A, M](schema: Apply[S, M])
    def toProductWith(f: HMap[M] => HMap[context.product.Attributes]): Product.Of[S, A] =
      self.apply(schema.self.toProduct, f(schema.metadata))

    def toProduct: Product.Of[S, A] = toProductWith(_ => context.product.default)

    def apply[B](key: Key[B] & Singleton & M): B = schema.metadata.apply(key)
    def apply[B](key: Key[B] & Singleton & M, value: B): Apply[S, M] =
      schema.copy(metadata = schema.metadata.put(key, value))
    @targetName("set")
    def apply[B](key: Key[Option[B]] & Singleton & M, value: B): Apply[S, M] =
      schema.copy(metadata = schema.metadata.put(key, Some(value)))
    def clear[B](key: Key[Option[B]] & Singleton & M): Apply[S, M] =
      schema.copy(metadata = schema.metadata.put(key, None))
