package io.taig.otter

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*

import scala.annotation.tailrec
import io.taig.otter.Field.Modify
import io.taig.otter.Field.Root
import io.taig.otter.Field.Optional

object CirceJsonEncoder extends Encoder[Json, CirceJson]:
  override def apply[A](codec: Json[A], a: A): CirceJson = codec match
    case Json.Collection(value) => apply(codec = value, a)
    case Json.Constant(value)   => apply(codec = value, a)
    case Json.Dictionary(value) => CirceJson.fromFields(apply(codec = value, a))
    case Json.Enumeration(self) => apply(codec = self, a)
    case Json.Nullable(value)   => apply(codec = value, a)
    case Json.Primitive(value)  => apply(codec = value, a)
    case Json.Record(value)     => CirceJson.fromFields(apply(codec = value, a))
    case Json.Tuple(value)      => CirceJson.fromValues(apply(codec = value, a))
    // case Json.Union(self)       => apply(codec = self, a)

  @tailrec
  def apply[A](codec: Collection[Json, A], a: A): CirceJson = codec match
    case Collection.Indexed(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Linked(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Constant[Json, A], a: A): CirceJson = codec match
    case Constant.Root(reference, _, _) => apply(codec = reference.self.value, reference.value)
    case Constant.Modify(self, _, g)    => apply(codec = self, g(a))

  def apply[A](codec: Dictionary[Json.Key, Json, A], a: A): List[(String, CirceJson)] = codec match
    case Dictionary.Root(key, value, _, _, _) =>
      a.map((a, b) => (JsonKeyPrinter(codec = key.value, a), apply(codec = value.value, b)))
    case Dictionary.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Enumeration[Json.Primitive, A], a: A): CirceJson = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

  @tailrec
  def apply[A](codec: Nullable[Json, A], a: A): CirceJson = codec match
    case Nullable.Default(codec, _, _) => apply(codec = codec.value, a)
    case Nullable.Modify(self, _, g)   => apply(codec = self, g(a))
    case Nullable.Root(codec, _)       => a.fold(CirceJson.Null)(apply(codec = codec.value, _))
    case Nullable.Void(_)              => CirceJson.Null

  @tailrec
  def apply[A](codec: Primitive[A], a: A): CirceJson = codec match
    case _: Primitive.Boolean.Root                            => CirceJson.fromBoolean(a)
    case _: Primitive.Number.BigDecimal                       => CirceJson.fromBigDecimal(BigDecimal(a))
    case _: Primitive.Number.BigInteger                       => CirceJson.fromBigInt(BigInt(a))
    case _: Primitive.Number.Double                           => CirceJson.fromDoubleOrString(a)
    case _: Primitive.Number.Float                            => CirceJson.fromFloatOrString(a)
    case _: Primitive.Number.Int                              => CirceJson.fromInt(a)
    case _: Primitive.Number.Long                             => CirceJson.fromLong(a)
    case _: Primitive.String.Text                             => CirceJson.fromString(a)
    case Primitive.Boolean.Modify(self, _, g)                 => apply(codec = self, g(a))
    case Primitive.String.Modify(self, _, g)                  => apply(codec = self, g(a))
    case Primitive.Number.Modify(self, _, g)                  => apply(codec = self, g(a))
    case Primitive.String.Parser(name, _, encode, _, _, _, _) => CirceJson.fromString(encode(a))

  def apply[A](codec: Record[Json.Field, A], a: A): List[(String, CirceJson)] = codec match
    case Record.Empty(_)            => Nil
    case Record.Root(field, _)      => apply(codec = field.value, a)
    case Record.Modify(self, _, g)  => apply(codec = self, g(a))
    case Record.Optional(self)      => a.fold(Nil)(apply(codec = self, _))
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Field[Json.Key, Json, A], a: A): List[(String, CirceJson)] = codec match
    case Modify(self, f, g) => apply(codec = self, g(a))
    case Root(key, value, _) =>
      (
        JsonKeyPrinter(codec = key.self.value, key.value),
        apply(codec = value.value, a)
      ) :: Nil
    case Optional(self) => a.fold(Nil)(apply(codec = self, _))

  def apply[A](codec: Tuple[Json, A], a: A): List[CirceJson] = codec match
    case _: Tuple.Empty            => Nil
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a = a._1) ++ apply(codec = right, a = a._2)
    case Tuple.Root(codec, _)      => List(apply(codec = codec.value, a))

  // def apply[A](codec: Union[Json, A], a: A): CirceJson = codec match
  //   case codec: Union.Untagged[Json, A] => apply(codec, a, discriminator = none)
  //   case codec: Union.Tagged[Json, A]   => apply(codec = codec.untagged, a, discriminator = codec.discriminator.some)

  // def apply[A](codec: Union.Untagged[Json, A], a: A, discriminator: Option[Discriminator]): CirceJson = codec match
  //   case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a), discriminator)
  //   case Union.Untagged.Branch(name, codec, _) => apply(name, codec = codec.value, a, discriminator)
  //   case Union.Untagged.OrElse(left, right, _) =>
  //     a.fold(apply(codec = left, _, discriminator), apply(codec = right, _, discriminator))

  def apply[A](name: String, codec: Json[A], a: A, discriminator: Option[Discriminator]): CirceJson =
    discriminator match
      case Some(Discriminator.Explicit(identifier, value)) =>
        CirceJson.obj(identifier := name, value := apply(codec = codec, a))
      case Some(Discriminator.Merged(identifier)) =>
        apply(codec = codec, a).deepMerge(CirceJson.obj(identifier := name))
      case Some(Discriminator.Keyed) => CirceJson.obj(name := apply(codec = codec, a))
      case None                      => apply(codec = codec, a)
