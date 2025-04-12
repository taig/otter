package io.taig.otter.http

import cats.syntax.all.*
import scala.annotation.tailrec
import io.taig.otter.*
import cats.data.Chain

final class HttpSegmentPrinter(explode: Boolean, style: Header.Style):
  def apply[A](name: String, codec: Http.Segment[A], a: A): String = codec match
    case codec: Http.Segment.Array[A]  => apply(name, codec, a)
    case codec: Http.Segment.Object[A] => apply(name, codec, a)
    case codec: Http.Segment.Value[A]  => apply(name, codec, a)

  def apply[A](name: String, codec: Http.Segment.Array[A], a: A): String =
    val values = apply(codec, a)

    (explode, style) match
      case (_, Header.Style.Simple)     => values.map(escape(_, ",")).mkString_(",")
      case (false, Header.Style.Label)  => s".${values.map(escape(_, ",")).mkString_(",")}"
      case (true, Header.Style.Label)   => s".${values.map(escape(_, ".")).mkString_(".")}"
      case (false, Header.Style.Matrix) => s";${escape(name, "=")}=${values.map(escape(_, ",")).mkString_(",")}"
      case (true, Header.Style.Matrix) =>
        values.map(value => s";${escape(name, List(";", "="))}=${escape(value, ";")}").mkString_("")

  def apply[A](codec: Http.Segment.Array[A], a: A): Chain[String] = codec match
    case Http.Segment.Array.Collection(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Segment.Array.Tuple(self)      => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Collection[Http.Segment.Value, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(apply(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(apply(codec = codec.value, _))
    case Collection.Modify(self, _, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Http.Segment.Value, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(apply(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](name: String, codec: Http.Segment.Object[A], a: A): String =
    val values = apply(codec, a)

    (explode, style) match
      case (false, Header.Style.Simple) =>
        values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")
      case (true, Header.Style.Simple) =>
        values.map((name, value) => s"${escape(name, List(",", "="))}=${escape(value, ",")}").mkString_(",")
      case (false, Header.Style.Label) =>
        s".${values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")}"
      case (true, Header.Style.Label) =>
        s".${values.map((name, value) => s"${escape(name, List(".", "="))}=${escape(value, ".")}").mkString_(".")}"
      case (false, Header.Style.Matrix) =>
        s";$name=${values.map((name, value) => s"${escape(name, ",")},${escape(value, ",")}").mkString_(",")}"
      case (true, Header.Style.Matrix) =>
        s";${values.map((name, value) => s"${escape(name, "=")}=${escape(value, ";")}").mkString_(";")}"

  def apply[A](codec: Http.Segment.Object[A], a: A): Chain[(String, String)] = codec match
    case Http.Segment.Object.Dictionary(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Segment.Object.Record(self)     => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Dictionary[Http.Segment.Value, Http.Segment.Value, A], a: A): List[(String, String)] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a.map((name, value) => (apply(codec = key.value, name), apply(codec = codec.value, value)))
    case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Http.Segment.Value, Http.Segment.Value, A], a: A): Chain[(String, String)] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, codec, _) =>
      val name = apply(codec = key.self.value, key.value)
      val value = apply(codec = codec.value, a)
      Chain.one((name, value))
    case Record.Modify(self, f, g)         => apply(codec = self, g(a))
    case Record.Optional(self)             => a.fold(Chain.empty)(apply(codec = self, _))
    case Record.Zip(left, right, metadata) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](name: String, codec: Http.Segment.Value[A], a: A): String =
    val value = apply(codec, a)

    style match
      case Header.Style.Simple => value
      case Header.Style.Label  => s".$value"
      case Header.Style.Matrix => s";${escape(name, "=")}=$value"

  def apply[A](codec: Http.Segment.Value[A], a: A): String = codec match
    case Http.Segment.Value.Constant(self)    => apply(codec = self, a)
    case Http.Segment.Value.Enumeration(self) => apply(codec = self, a)
    case Http.Segment.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Http.Segment.Value.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Constant[Http.Segment.Value.Primitive, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(reference, _) => apply(codec = reference.self.value, reference.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Segment.Value.Primitive, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Http.Segment.Value, A], a: A): String = codec match
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))
