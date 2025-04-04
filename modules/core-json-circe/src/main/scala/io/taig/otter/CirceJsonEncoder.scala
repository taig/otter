package io.taig.otter

import io.circe.Json as CirceJson
import io.circe.syntax.*
import cats.syntax.all.*
import scala.annotation.tailrec

object CirceJsonEncoder extends Encoder[Json, CirceJson]:
  override def apply[A](codec: Json[A], a: A): CirceJson = codec match
    case Json.Collection(value)   => apply(codec = value, a)
    case Json.Constant(value)     => apply(codec = value, a)
    case Json.Dictionary(value)   => CirceJson.fromFields(apply(codec = value, a))
    case Json.Enumeration(value)  => apply(codec = value, a)
    case Json.Optional(value)     => apply(codec = value, a)
    case codec: Json.Primitive[A] => apply(codec = codec.value, a)
    case Json.Record(value)       => CirceJson.fromFields(apply(codec = value, a))
    case Json.Tuple(value)        => CirceJson.fromValues(apply(codec = value, a))
    case Json.Union(value)        => ???

    // case codec: Union[Json.Key, Json, A]      => apply(codec, a)

  @tailrec
  def apply[A](codec: Collection[Json, A], a: A): CirceJson = codec match
    case Collection.Indexed(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Linked(self, _, _, _, _) =>
      CirceJson.fromValues(a.map(apply(self.value, _)))
    case Collection.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Constant[Json, A], a: A): CirceJson = codec match
    case Constant.Root(reference, _) => apply(codec = reference.self.value, reference.value)
    case Constant.Modify(self, _, g) => apply(codec = self, g(a))

  def apply[A](codec: Dictionary[Json.Key, Json, A], a: A): List[(String, CirceJson)] = codec match
    case Dictionary.Root(key, value, _, _, _) =>
      a.map { case (a, b) =>
        (CirceJsonKeyEncoder(codec = key.value, a), apply(codec = value.value, b))
      }
    case Dictionary.Modify(self, _, g) => apply(codec = self, g(a))

  @tailrec
  def apply[A](codec: Enumeration[Json.Primitive, A], a: A): CirceJson = codec match
    case Enumeration.Modify(self, _, g)      => apply(codec = self, g(a))
    case Enumeration.Root(codec, mapping, _) => apply(codec = codec.value, mapping(a))

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

  def apply[A](codec: Record[Json.Field, A], a: A): List[(String, CirceJson)] = codec match
    case Record.Empty(_)            => Nil
    case Record.Modify(self, _, g)  => apply(codec = self, g(a))
    case Record.Root(field, _)      => apply(field = field.value, a).toList
    case Record.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)

  def apply[A](codec: Tuple[Json, A], a: A): List[CirceJson] = codec match
    case _: Tuple.Empty            => Nil
    case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
    case Tuple.Zip(left, right, _) => apply(codec = left, a = a._1) ++ apply(codec = right, a = a._2)
    case Tuple.Root(codec, _)      => List(apply(codec = codec.value, a))

  // def apply[A](codec: Union[Json.Key, Json, A], a: A): CirceJson = codec match
  //   case codec: Union.Untagged[Json.Key, Json, A] => apply(codec = codec, a, discriminator = none)
  //   case codec: Union.Tagged[Json.Key, Json, A]   => apply(codec = codec, a)

  // def apply[A](codec: Union.Untagged[Json.Key, Json, A], a: A, discriminator: Option[Discriminator]): CirceJson =
  //   codec match
  //     case Union.Untagged.Modify(self, _, g)     => apply(codec = self, g(a))
  //     case Union.Untagged.Root(branch, _)        => apply(branch, a, discriminator)
  //     case Union.Untagged.OrElse(left, right, _) => a.fold(apply(codec = left, _), apply(codec = right, _))

  // def apply[A](codec: Union.Tagged[Json.Key, Json, A], a: A): CirceJson = codec match
  //   case Union.Tagged.Keyed(untagged) => apply(codec = untagged, a, discriminator = Discriminator.Keyed.some)
  //   case Union.Tagged.Merged(untagged, discriminator) =>
  //     apply(codec = untagged, a, discriminator = discriminator.some)
  //   case Union.Tagged.Nested(untagged, discriminator) =>
  //     apply(codec = untagged, a, discriminator = discriminator.some)

  // @tailrec
  // def apply[A](branch: Branch[Json.Key, Json, A], a: A, discriminator: Option[Discriminator]): CirceJson = branch match
  //   case Branch.Root(key, codec, _) =>
  //     discriminator match
  //       case Some(Discriminator.Nested(identifier, value)) =>
  //         CirceJson.obj(
  //           identifier := CirceJsonKeyEncoder(codec = key.self.value, key.value),
  //           value := apply(codec = codec.value, a)
  //         )
  //       case Some(Discriminator.Merged(identifier)) =>
  //         apply(codec = codec.value, a)
  //           .deepMerge(CirceJson.obj(identifier := CirceJsonKeyEncoder(codec = key.self.value, key.value)))
  //       case Some(Discriminator.Keyed) =>
  //         CirceJson.obj(CirceJsonKeyEncoder(codec = key.self.value, key.value) := apply(codec = codec.value, a))
  //       case None => apply(codec = codec.value, a)
  //   case Branch.Modify(self, _, g) => apply(branch = self, a = g(a), discriminator)

  def apply[A, B](field: Json.Field[A], a: A): Option[(String, CirceJson)] = ???
  // field match
  //   case Json.Field.Required(key, value, _) =>
  //     (CirceJsonKeyEncoder(codec = key.self.value, key.value), apply(codec = value.value, a)).some
  //   case Field.Required.Modify(self, _, g) => apply(field = self, g(a))
  //   case Field.Modify(self, _, g)          => apply(field = self, g(a))
  //   case Field.Default(self, _)            => apply(field = self, a)
  //   case Field.Optional(self)              => a.flatMap(apply(field = self, _))
