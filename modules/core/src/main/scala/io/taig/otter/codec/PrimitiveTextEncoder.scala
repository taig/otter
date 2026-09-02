package io.taig.otter.codec

import io.taig.otter.Primitive

/** Writes a primitive as text, which is what a position with no type of its own needs: a CSV cell, a dictionary key, a
  * field's name.
  *
  * Every primitive writes as its own text, which is why the leaves share one branch: such a position has no type for a
  * number or a boolean to be written as, so there is nothing to choose between them. Only a [[Primitive.Text.Format]]
  * writes something other than the value's own rendering.
  *
  * Demanded as `Encoder[Primitive.Text, String]` wherever only text may appear, which the contravariance of [[Encoder]]
  * grants for free.
  */
object PrimitiveTextEncoder extends Encoder[Primitive, String]:
  override def encode[W](schema: Primitive[W, Any], w: W): String = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => print(w)
    case Primitive.Boolean.Root | Primitive.Number.BigDecimal(_) | Primitive.Number.BigInteger(_) |
        Primitive.Number.Double(_) | Primitive.Number.Float(_) | Primitive.Number.Int(_) | Primitive.Number.Long(_) |
        Primitive.Text.Root(_) =>
      w.toString
