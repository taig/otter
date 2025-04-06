package io.taig.otter.http

import io.taig.otter.*
import cats.syntax.all.*
import scala.annotation.tailrec
import io.taig.otter.Record.Empty
import io.taig.otter.Record.Field
import io.taig.otter.Record.Modify
import io.taig.otter.Record.Zip
import cats.data.Chain

final class HeaderCodecObjectPrinter(explode: Boolean) extends Printer[Header.Codec.Object]:
  override def apply[A](codec: Header.Codec.Object[A], a: A): String = codec match
    case Header.Codec.Object.Dictionary(self) => apply(codec = self, a).mkString_(",")
    case Header.Codec.Object.Record(self)     => apply(codec = self, a).mkString_(",")

  @tailrec
  def apply[A](codec: Dictionary[Header.Codec, Header.Codec, A], a: A): List[String] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a
        .map: (name, value) =>
          (HeaderCodecPrinter(codec = key.value, name), HeaderCodecPrinter(codec = codec.value, value))
        .map: (name, value) =>
          if explode then s"$name=$value"
          else s"$name,$value"
    case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Header.Codec, Header.Codec, A], a: A): Chain[String] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = HeaderCodecPrinter(codec = key.self.value, key.value)
      val value = HeaderCodecPrinter(codec = codec.value, a)
      val result = if explode then s"$name=$value" else s"$name,$value"
      Chain.one(result)
    case Record.Modify(self, f, g)         => apply(codec = self, g(a))
    case Record.Optional(self)             => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, metadata) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
