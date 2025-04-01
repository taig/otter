package io.taig.otter

import io.circe.Json as CirceJson
import io.circe.syntax.*
import cats.syntax.all.*
import scala.annotation.tailrec

object CirceJsonEncoder:
  def apply[A](codec: Json[A], a: A): CirceJson = codec.self match
    case codec: Collection[Json, A]           => apply(codec, a)
    case codec: Constant[Json, A]             => apply(codec, a)
    case codec: Dictionary[Json.Key, Json, A] => CirceJson.fromFields(apply(codec, a))
    case codec: Enumeration[A]                => apply(codec, a)
    case codec: Optional[Json, A]             => apply(codec, a)
    case codec: Primitive[A]                  => apply(codec, a)
    case codec: Record[Json.Key, Json, A]     => CirceJson.fromFields(apply(codec, a))
    case codec: Tuple[Json, A]                => CirceJson.fromValues(apply(codec, a))
    case codec: Union[Json.Key, Json, A]      => apply(codec, a)

  @tailrec
  def apply[A](codec: Collection[Json, A], a: A): CirceJson = codec match
    case Collection.Indexed(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Linked(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Constant[Json, A], a: A): CirceJson = codec match
    case Constant.Root(codec, reference, _) => apply(codec = codec.value, reference)
    case Constant.Modify(self, _, g)        => apply(codec = self, g(a))

  def apply[A](codec: Dictionary[Json.Key, Json, A], a: A): List[(String, CirceJson)] = codec match
    case Dictionary.Root(key, value, _, _, _) =>
      a.map { case (a, b) =>
        (CirceJsonKeyEncoder(codec = key.value, a), apply(codec = value.value, b))
      }
    case Dictionary.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Enumeration[A], a: A): CirceJson = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec, mapping(a))

  @tailrec
  def apply[A](codec: Optional[Json, A], a: A): CirceJson = codec match
    case Optional.Default(codec, _, _) => apply(codec = codec.value, a)
    case Optional.Modify(self, _, g)   => apply(codec = self, g(a))
    case Optional.Nullable(codec, _)   => a.fold(CirceJson.Null)(apply(codec = codec.value, _))

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

  def apply[A](codec: Record[Json.Key, Json, A], a: A): List[(String, CirceJson)] = codec match
    case Record.Empty(_)            => Nil
    case Record.Modify(self, _, g)  => apply(codec = self, g(a))
    case Record.Root(field, _)      => apply(field, a).toList
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Tuple[Json, A], a: A): List[CirceJson] = codec match
    case _: Tuple.Empty                => Nil
    case Tuple.Modify(self, _, g)      => apply(codec = self, g(a))
    case Tuple.Prepend(self, codec, _) => apply(codec = codec.value, a = a.head) :: apply(codec = self, a = a.tail)
    case Tuple.Root(codec, _)          => List(apply(codec = codec.value, a))

  def apply[A](codec: Union[Json.Key, Json, A], a: A): CirceJson = codec match
    case codec: Union.Untagged[Json.Key, Json, A] => apply(codec = codec, a, discriminator = none)
    case codec: Union.Tagged[Json.Key, Json, A]   => apply(codec = codec, a)

  def apply[A](codec: Union.Untagged[Json.Key, Json, A], a: A, discriminator: Option[Discriminator]): CirceJson =
    codec match
      case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
      case Union.Untagged.Root(branch, _)        => apply(branch, a, discriminator)
      case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  def apply[A](codec: Union.Tagged[Json.Key, Json, A], a: A): CirceJson = codec match
    case Union.Tagged.Keyed(untagged) => apply(codec = untagged, a, discriminator = Discriminator.Keyed.some)
    case Union.Tagged.Merged(untagged, discriminator) =>
      apply(codec = untagged, a, discriminator = discriminator.some)
    case Union.Tagged.Nested(untagged, discriminator) =>
      apply(codec = untagged, a, discriminator = discriminator.some)

  @tailrec
  def apply[A](branch: Branch[Json.Key, Json, A], a: A, discriminator: Option[Discriminator]): CirceJson = branch match
    case Branch.Root(name, codec, _) =>
      discriminator match
        case Some(Discriminator.Nested(identifier, value)) =>
          CirceJson.obj(identifier := "TODO", value := apply(codec = codec.value, a))
        case Some(Discriminator.Merged(identifier)) =>
          apply(codec = codec.value, a).deepMerge(CirceJson.obj(identifier := "TODO"))
        case Some(Discriminator.Keyed) =>
          CirceJson.obj("TODO" := apply(codec = codec.value, a))
        case None => apply(codec = codec.value, a)
    case Branch.Modify(self, _, g) => apply(branch = self, a = g(a), discriminator)

  def apply[A, B](field: Field[Json.Key, Json, A], a: A): Option[(String, CirceJson)] = field match
    case Field.Required.Root(key, value, _) =>
      // (CirceJsonKeyEncoder(codec = key.value, name), apply(codec = codec.value, b)).some
      ???
    case Field.Required.Modify(self, _, g) => apply(field = self, g(a))
    case Field.Modify(self, _, g)          => apply(field = self, g(a))
    case Field.Default(self, _)            => apply(field = self, a)
    case Field.Optional(self)              => a.flatMap(apply(field = self, _))
