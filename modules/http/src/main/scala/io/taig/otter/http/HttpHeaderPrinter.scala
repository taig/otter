package io.taig.otter.http

import io.taig.otter.*
import scala.annotation.tailrec
import cats.data.Chain
import cats.syntax.all.*

final class HttpHeaderPrinter(explode: Boolean) extends Printer[Http.Header]:
  override def apply[A](codec: Http.Header[A], a: A): String = codec match
    case codec: Http.Header.Array[A]  => array(apply(codec, a))
    case codec: Http.Header.Object[A] => obj(values = apply(codec, a))
    case codec: Http.Header.Value[A]  => apply(codec, a)

  def apply[A](codec: Http.Header.Array[A], a: A): Chain[String] = codec match
    case Http.Header.Array.Collection(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Header.Array.Tuple(self)      => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Collection[Http.Header.Value, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(apply(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(apply(codec = codec.value, _))
    case Collection.Modify(self, f, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Http.Header.Value, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(apply(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Http.Header.Object[A], a: A): Chain[(String, String)] = codec match
    case Http.Header.Object.Dictionary(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Header.Object.Record(self)     => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Dictionary[Http.Header.Value, Http.Header.Value, A], a: A): List[(String, String)] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a.map: (name, value) =>
        (apply(codec = key.value, name), apply(codec = codec.value, value))
    case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Http.Header.Value, Http.Header.Value, A], a: A): Chain[(String, String)] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = apply(codec = key.self.value, key.value)
      val value = apply(codec = codec.value, a)
      Chain.one((name, value))
    case Record.Modify(self, f, g)  => apply(codec = self, g(a))
    case Record.Optional(self)      => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Http.Header.Value[A], a: A): String = codec match
    case Http.Header.Value.Constant(self)    => apply(codec = self, a)
    case Http.Header.Value.Enumeration(self) => apply(codec = self, a)
    case Http.Header.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Http.Header.Value.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Http.Header.Value.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(codec, _)     => apply(codec = codec.self.value, codec.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Header.Value.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Http.Header.Value, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  def array(values: Chain[String]): String = values.map(escape(_, ",")).mkString_(",")

  def obj(values: Chain[(String, String)]): String =
    if explode then
      val characters = List("=", ",")
      values.map((name, value) => s"${escape(name, characters)}=${escape(value, characters)}").mkString_(",")
    else values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")

object HttpHeaderPrinter:
  def apply(explode: Boolean): Printer[Http.Header] = new HttpHeaderPrinter(explode)
