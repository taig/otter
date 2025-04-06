package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import scala.annotation.tailrec
import io.taig.otter.Collection.Linked
import io.taig.otter.Collection.Modify
import io.taig.otter.Tuple.Empty
import io.taig.otter.Tuple.Root
import io.taig.otter.Tuple.Zip
import cats.data.Chain

// TODO escape commas
object HeaderCodecArrayPrinter extends Printer[Header.Codec.Array]:
  override def apply[A](codec: Header.Codec.Array[A], a: A): String = codec match
    case Header.Codec.Array.Collection(self) => apply(codec = self, a).mkString_(",")
    case Header.Codec.Array.Tuple(self)      => apply(codec = self, a).mkString_(",")

  @tailrec
  def apply[A](codec: Collection[Header.Codec, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(HeaderCodecPrinter(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(HeaderCodecPrinter(codec = codec.value, _))
    case Collection.Modify(self, f, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Header.Codec, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(HeaderCodecPrinter(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
