package io.taig.otter.codec

import cats.Monoid
import cats.syntax.all.*
import io.taig.otter.Record

/** Writes a record as what its fields contribute, combined in declaration order.
  *
  * `M` is whatever one field writes and a record combines. A format building a document out of values asks for a
  * container of pairs and hands it to its own object constructor; a format writing into an output asks for the write
  * itself, and then combining two fields is composing two writes and no container is built at all.
  */
final class RecordEncoder[F[-_, +_], M: Monoid](encoder: Encoder[F, M]) extends Encoder[[w, r] =>> Record[F, w, r], M]:
  override def encode[W](schema: Record[F, W, Any], w: W): M = schema match
    case Record.Empty                => Monoid[M].empty
    case Record.Modify(self, _, g)   => encode(self, g(w))
    case Record.Product(left, right) => encode(left, w._1) |+| encode(right, w._2)
    case Record.Root(field)          => encoder.encode(field.value, w)
