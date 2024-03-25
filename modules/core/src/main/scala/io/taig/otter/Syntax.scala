package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.Key
import io.taig.hmap.HMap
import scala.annotation.targetName

trait Syntax[C <: Context] extends Types[C]:
  self =>

  extension [S <: Plain.Schema[A], A, C <: context.Schema.Metadata[M], M](schema: Apply[S, C, M])
    def toProductWith(f: HMap[M] => HMap[context.Product]): Product.Of[S, A] =
      self.apply(schema.self.toProduct, Metadata(context.product, f(schema.metadata.values)))

    def toProduct: Product.Of[S, A] = toProductWith(schema.metadata.context.toProduct)

    def apply[B](key: Key[B] & Singleton & M): B = schema.metadata.values.apply(key)
    def apply[B](key: Key[B] & Singleton & M, value: B): Apply[S, C, M] =
      schema.copy(metadata = schema.metadata.copy(values = schema.metadata.values.put(key, value)))
    @targetName("set")
    def apply[B](key: Key[Option[B]] & Singleton & M, value: B): Apply[S, C, M] = ???
    // schema.copy(metadata = schema.metadata.put(key, Some(value)))
    def clear[B](key: Key[Option[B]] & Singleton & M): Apply[S, C, M] = ???
    // schema.copy(metadata = schema.metadata.put(key, None))
