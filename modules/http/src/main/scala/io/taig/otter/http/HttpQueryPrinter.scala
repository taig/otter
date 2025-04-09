package io.taig.otter.http

import io.taig.otter.*
import cats.data.Chain
import cats.syntax.all.*
import scala.annotation.tailrec

object HttpQueryPrinter:
  def apply[A](
      name: String,
      codec: Http.Query[A],
      a: A,
      explode: Boolean,
      style: Query.Style
  ): Chain[(String, Option[String])] = codec match
    case codec: Http.Query.Array[?] =>
      array(name, values = apply(codec, a), explode, style).map(_.map(_.some))
    case codec: Http.Query.Object[?] =>
      obj(name, values = apply(codec, a), explode, style).map(_.map(_.some))
    case codec: Http.Query.Optional[?] => apply(name, codec, a, explode, style)
    case codec: Http.Query.Value[?]    => Chain.one((name, apply(codec, a).some))

  def apply[A](codec: Http.Query.Array[A], a: A): Chain[String] = codec match
    case Http.Query.Array.Collection(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Query.Array.Tuple(self)      => apply(codec = self, a)

  def apply[A](codec: Collection[Http.Query.Value, A], a: A): Seq[String] = codec match
    case Collection.Indexed(codec, _, _, _, _) => a.map(apply(codec = codec.value, _))
    case Collection.Linked(codec, _, _, _, _)  => a.map(apply(codec = codec.value, _))
    case Collection.Modify(self, _, g)         => apply(codec = self, g(a))

  def apply[A](codec: Tuple[Http.Query.Value, A], a: A): Chain[String] = codec match
    case Tuple.Empty(_)            => Chain.empty
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Root(codec, _)      => Chain.one(apply(codec = codec.value, a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Http.Query.Object[A], a: A): Chain[(String, String)] = codec match
    case Http.Query.Object.Dictionary(self) => Chain.fromSeq(apply(codec = self, a))
    case Http.Query.Object.Record(self)     => apply(codec = self, a)

  def apply[A](codec: Dictionary[Http.Query.Value, Http.Query.Value, A], a: A): List[(String, String)] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      a.map((name, value) => (apply(codec = key.value, name), apply(codec = codec.value, value)))
    case Dictionary.Modify(self, _, g) => apply(codec = self, g(a))

  def apply[A](codec: Record[Http.Query.Value, Http.Query.Value, A], a: A): Chain[(String, String)] = codec match
    case Record.Empty(_) => Chain.empty
    case Record.Field(key, value, _) =>
      Chain.one((apply(codec = key.self.value, key.value), apply(codec = value.value, a)))
    case Record.Modify(self, _, g)  => apply(codec = self, g(a))
    case Record.Optional(self)      => a.map(apply(codec = self, _)).getOrElse(Chain.empty)
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](
      name: String,
      codec: Http.Query.Optional[A],
      a: A,
      explode: Boolean,
      style: Query.Style
  ): Chain[(String, Option[String])] = apply(name, codec = codec.self, a, explode, style)

  def apply[A](
      name: String,
      codec: Optional[Http.Query, A],
      a: A,
      explode: Boolean,
      style: Query.Style
  ): Chain[(String, Option[String])] = codec match
    case Optional.Default(codec, _, _) => apply(name, codec = codec.value, a, explode, style)
    case Optional.Modify(self, f, g)   => apply(name, codec = self, g(a), explode, style)
    case Optional.Nullable(codec, _) =>
      a.fold(Chain.one((name, none)))(apply(name, codec = codec.value, _, explode, style))

  def apply[A](codec: Http.Query.Value[A], a: A): String = codec match
    case Http.Query.Value.Constant(self)    => apply(codec, a)
    case Http.Query.Value.Enumeration(self) => apply(codec, a)
    case Http.Query.Value.Primitive(self)   => PrimitivePrinter.Unquoted(codec = self, a)
    case Http.Query.Value.Union(self)       => apply(codec, a)

  @tailrec
  def apply[A](codec: Constant[Http.Query.Value, A], a: A): String = codec match
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))
    case Constant.Root(codec, _)     => apply(codec = codec.self.value, codec.value)

  @tailrec
  def apply[A](codec: Enumeration[Http.Query.Value, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  def apply[A](codec: Union.Untagged[Http.Query.Value, A], a: A): String = codec match
    case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
    case Union.Untagged.Branch(_, codec, _)    => apply(codec = codec.value, a)
    case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  def array[A](name: String, values: Chain[String], explode: Boolean, style: Query.Style): Chain[(String, String)] =
    (explode, style) match
      case (true, _)                       => values.tupleLeft(name)
      case (_, Query.Style.Form)           => Chain.one((name, values.mkString_(",")))
      case (_, Query.Style.SpaceDelimited) => Chain.one((name, values.mkString_(" ")))
      case (_, Query.Style.PipeDelimited)  => Chain.one((name, values.mkString_("|")))

  def obj[A](
      name: String,
      values: Chain[(String, String)],
      explode: Boolean,
      style: Query.Style
  ): Chain[(String, String)] =
    (explode, style) match
      case (false, Query.Style.Form) => Chain.one((name, values.map((key, value) => s"$key,$value").mkString_(",")))
      case (_, _)                    => values
