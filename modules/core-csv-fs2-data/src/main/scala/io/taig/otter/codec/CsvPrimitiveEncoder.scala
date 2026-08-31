package io.taig.otter.codec

import io.taig.otter.Csv
import io.taig.otter.Primitive

object CsvPrimitiveEncoder extends Encoder[Csv.Primitive.Node, String]:
  override def encode[W](csv: Csv.Primitive.Node[W, Any], w: W): String = csv match
    case Csv.Primitive.Boolean.Schema(annotation) => encode(annotation.self, w)
    case Csv.Primitive.Number.Schema(annotation)  => encode(annotation.self, w)
    case Csv.Primitive.Text.Schema(annotation)    => encode(annotation.self, w)

  /** Every primitive CSV has writes as its own text, which is why the leaves share one branch: a cell has no type for a
    * number or a boolean to be written as, so there is nothing to choose between them. Only a [[Primitive.Text.Format]]
    * writes something other than the value's own rendering.
    */
  def encode[W](schema: Primitive[W, Any], w: W): String = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => print(w)
    case Primitive.Boolean.Root | Primitive.Number.BigDecimal(_) | Primitive.Number.BigInteger(_) |
        Primitive.Number.Double(_) | Primitive.Number.Float(_) | Primitive.Number.Int(_) | Primitive.Number.Long(_) |
        Primitive.Text.Root(_) =>
      w.toString
