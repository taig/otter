package io.taig.otter.http

import io.taig.otter.*
import cats.syntax.all.*
import scala.annotation.tailrec
import io.taig.otter.Record.Empty
import io.taig.otter.Record.Field
import io.taig.otter.Record.Modify
import io.taig.otter.Record.Zip
import cats.data.Chain

// TODO escape
final class HttpHeaderObjectPrinter(explode: Boolean) extends Printer[Http.Header.Object]:
  override def apply[A](codec: Http.Header.Object[A], a: A): String = codec match
    case Http.Header.Object.Dictionary(self) => apply(codec = self, a).mkString_(",")
    case Http.Header.Object.Record(self)     => apply(codec = self, a).mkString_(",")

  @tailrec
  def apply[A](codec: Dictionary[Http.Header.Value, Http.Header.Value, A], a: A): List[String] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a
        .map: (name, value) =>
          (HttpHeaderValuePrinter(codec = key.value, name), HttpHeaderValuePrinter(codec = codec.value, value))
        .map(apply(_, _))
    case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Http.Header.Value, Http.Header.Value, A], a: A): Chain[String] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = HttpHeaderValuePrinter(codec = key.self.value, key.value)
      val value = HttpHeaderValuePrinter(codec = codec.value, a)
      Chain.one(apply(name, value))
    case Record.Modify(self, f, g)         => apply(codec = self, g(a))
    case Record.Optional(self)             => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, metadata) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply(name: String, value: String): String = if explode then s"$name=$value" else s"$name,$value"
