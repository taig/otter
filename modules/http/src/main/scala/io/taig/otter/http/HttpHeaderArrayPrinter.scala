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

// TODO escape
object HttpHeaderArrayPrinter extends Printer[Http.Header.Array]:
  override def apply[A](codec: Http.Header.Array[A], a: A): String = codec match
    case Http.Header.Array.Collection(self) => apply(codec = self, a).mkString_(",")
    case Http.Header.Array.Tuple(self)      => apply(codec = self, a).mkString_(",")

  @tailrec
  def apply[A](codec: Collection[Http.Header.Value, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(HttpHeaderValuePrinter(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(HttpHeaderValuePrinter(codec = codec.value, _))
    case Collection.Modify(self, f, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Http.Header.Value, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(HttpHeaderValuePrinter(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
