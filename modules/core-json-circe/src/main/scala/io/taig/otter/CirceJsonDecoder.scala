package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Decoder as CirceDecoder
import io.circe.Json as CirceJson

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Field.Root
import io.taig.otter.Field.Optional
import io.taig.otter.codec.Decoder

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](codec: Json[A], json: CirceJson): Validated[Violations, A] = codec match
    case Json.Collection(self)  => ??? // apply(codec = self, json)
    case Json.Constant(self)    => ??? // apply(codec = self, json)
    case Json.Dictionary(self)  => ??? // apply(codec = self, json)
    case Json.Enumeration(self) => ??? // apply(codec = self, json)
    case Json.Nullable(self)    => ??? // apply(codec = self, json)
    case Json.Primitive(self)   => ??? // apply(codec = self, json)
    case Json.Record(self)      => ??? // apply(codec = self, json)
    case Json.Tuple(self)       => ??? // apply(codec = self, json)
    // case Json.Union(self)       => apply(codec = self, json)

  // def apply[A](codec: Collection[Json, A], json: CirceJson): Validated[Violations, A] = codec match
  //   case codec.Collection.Indexed(codec, minimum, maximum, uniqueItems, _) =>
  //     apply(minimum, maximum, uniqueItems)(json).andThen: values =>
  //       values.zipWithIndex.traverse: (json, index) =>
  //         apply(codec = codec.value, json).leftMap(index /: _)
  //   case codec.Collection.Linked(codec, minimum, maximum, uniqueItems, _) =>
  //     apply(minimum, maximum, uniqueItems)(json).andThen: values =>
  //       values.toList.zipWithIndex.traverse: (json, index) =>
  //         apply(codec = codec.value, json).leftMap(index /: _)
  //   case codec.Collection.Modify(self, f, g) => apply(codec = self, json).map(f)

  // def apply(minimum: Option[Int], maximum: Option[Int], uniqueItems: Boolean)(
  //     json: CirceJson
  // ): Validated[Violations, Vector[CirceJson]] = json.asArray
  //   .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = toType(json))))
  //   .andThen: values =>
  //     val size = values.size

  //     minimum.traverse(minimum =>
  //       Validated.cond(
  //         test = size >= minimum,
  //         (),
  //         Violations.rootNec(
  //           Violation(constraint = Constraint.Collection.MinItems(reference = minimum), actual = size, hint = none)
  //         )
  //       )
  //     ) *> maximum.traverse(maximum =>
  //       Validated.cond(
  //         test = size <= maximum,
  //         (),
  //         Violations.rootNec(
  //           Violation(constraint = Constraint.Collection.MaxItems(reference = maximum), actual = size, hint = none)
  //         )
  //       )
  //     ) *> Validated
  //       .cond(
  //         test = uniqueItems && values.distinct.size == size,
  //         (),
  //         Violations.rootNec(
  //           Violation(constraint = Constraint.Collection.UniqueItems, actual = toValue(json), hint = none)
  //         )
  //       )
  //       .as(values)

  // def apply[A](codec: Constant[Json, A], json: CirceJson): Validated[Violations, A] = codec match
  //   case schema.Constant.Modify(self, f, _) => apply(codec = self, json).map(f)
  //   case schema.Constant.Root(codec, eq, _) =>
  //     apply(codec = codec.self.value, json).andThen: a =>
  //       Validated
  //         .cond(
  //           test = eq.eqv(a, codec.value),
  //           (),
  //           Violation.equal(
  //             reference = toValue(CirceJsonEncoder(codec = codec.self.value, codec.value)),
  //             actual = toValue(json)
  //           )
  //         )
  //         .leftMap(Violations.rootNec)

  // def apply[A](codec: Dictionary[Json.Key, Json, A], json: CirceJson): Validated[Violations, A] = codec match
  //   case Dictionary.Modify(self, f, _) => apply(codec = self, json).map(f)
  //   case Dictionary.Root(key, codec, minimum, maximum, _) =>
  //     json.asObject
  //       .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = toType(json))))
  //       .andThen: json =>
  //         val size = json.size

  //         minimum.traverse(minimum =>
  //           Validated.cond(
  //             test = size >= minimum,
  //             (),
  //             Violations.rootNec(
  //               Violation(constraint = Constraint.Object.MinProperties(reference = minimum), actual = size, hint = none)
  //             )
  //           )
  //         ) *> maximum.traverse(maximum =>
  //           Validated.cond(
  //             test = size <= maximum,
  //             (),
  //             Violations.rootNec(
  //               Violation(constraint = Constraint.Object.MaxProperties(reference = maximum), actual = size, hint = none)
  //             )
  //           )
  //         ) *> json.toList.traverse: (name, value) =>
  //           (
  //             JsonKeyParser(codec = key.value, value = name).leftMap(name /: _),
  //             apply(codec = codec.value, value).leftMap(name /: _)
  //           ).tupled

  // def apply[A](codec: Enumeration[Json.Primitive, A], json: CirceJson): Validated[Violations, A] = codec match
  //   case Enumeration.Modify(self, f, _) => apply(codec = self, json).map(f)
  //   case codec @ Enumeration.Root(reference, mapping, _) =>
  //     apply(codec = reference.value, json).andThen: value =>
  //       mapping
  //         .unapply(value)
  //         .toValid(
  //           Violations.rootNec(
  //             Violation.oneOf(
  //               values = codec.values.toList.map(mapping.apply).map(JsonPrimitivePrinter(codec = reference.value, _)),
  //               actual = toValue(json)
  //             )
  //           )
  //         )

  // def apply[A](codec: Nullable[Json, A], json: CirceJson): Validated[Violations, A] = codec match
  //   case Nullable.Modify(self, f, _) => apply(codec = self, json).map(f)
  //   case Nullable.Root(reference, _) =>
  //     if json.isNull then None.valid
  //     else apply(codec = reference.value, json).map(_.some)
  //   case Nullable.Default(reference, default, _) =>
  //     if json.isNull then default.valid
  //     else apply(codec = reference.value, json)
  //   case Nullable.Void(_) => ().valid

  // def apply[A](codec: Primitive[A], json: CirceJson): Validated[Violations, A] = codec match
  //   case _: Primitive.Boolean.Root            => apply[Boolean](name = "boolean", json)
  //   case _: Primitive.Number.BigDecimal       => apply[JBigDecimal](name = "bigDecimal", json)
  //   case _: Primitive.Number.BigInteger       => apply[JBigInteger](name = "bigInteger", json)
  //   case _: Primitive.Number.Double           => apply[Double](name = "double", json)
  //   case _: Primitive.Number.Float            => apply[Float](name = "float", json)
  //   case _: Primitive.Number.Int              => apply[Int](name = "int", json)
  //   case _: Primitive.Number.Long             => apply[Long](name = "long", json)
  //   case Primitive.Boolean.Modify(self, f, _) => apply(codec = self, json).map(f)
  //   case Primitive.Number.Modify(self, f, _)  => apply(codec = self, json).map(f)
  //   case Primitive.String.Modify(self, f, _)  => apply(codec = self, json).map(f)
  //   case Primitive.String.Parser(name, decode, _, _, _, _, _) =>
  //     apply[String](name = "string", json).andThen: value =>
  //       decode(value)
  //         .leftMap(error => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = error)))
  //         .toValidated
  //   case _: Primitive.String.Text => apply[String](name = "string", json)

  // def apply[A: CirceDecoder](name: String, json: CirceJson): Validated[Violations, A] = json
  //   .as[A]
  //   .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = failure.show)))
  //   .toValidated

  // // TODO support for rejecting additional properties
  // def apply[A](codec: Record[Json.Field, A], json: CirceJson): Validated[Violations, A] =
  //   json.asObject
  //     .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = toType(json))))
  //     .andThen(json => apply(codec, json = json.toList).map((_, a) => a))

  // def apply[A](
  //     codec: Record[Json.Field, A],
  //     json: List[(String, CirceJson)]
  // ): Validated[Violations, (List[(String, CirceJson)], A)] = codec match
  //   case Record.Empty(_)           => (json, ()).valid
  //   case Record.Root(field, _)     => apply(codec = field.value, json)
  //   case Record.Modify(self, f, _) => apply(codec = self, json).map(_.map(f))
  //   case Record.Optional(self) =>
  //     val lookup = json.map((key, _) => key).toSet

  //     val allKeysAbsent = codec.fields
  //       .map(_.value.key)
  //       .map(key => JsonKeyPrinter(codec = key.self.value, key.value))
  //       .forall(lookup.contains_)

  //     if allKeysAbsent then (json, none).valid[Violations]
  //     else apply(codec = self, json).map(_.map(_.some))
  //   case Record.Zip(left, right, _) =>
  //     apply(codec = left, json) match
  //       case Validated.Valid((json, a)) => apply(codec = right, json).map(_.tupleLeft(a))
  //       case Validated.Invalid(left) =>
  //         apply(codec = right, json) match
  //           case Validated.Valid(_)       => left.invalid
  //           case Validated.Invalid(right) => (left |+| right).invalid

  // def apply[A](
  //     codec: Field[Json.Key, Json, A],
  //     json: List[(String, CirceJson)]
  // ): Validated[Violations, (List[(String, CirceJson)], A)] = codec match
  //   case Field.Modify(self, f, g) => apply(codec = self, json).map(_.map(f))
  //   case Field.Root(key, codec, _) =>
  //     val name = JsonKeyPrinter(codec = key.self.value, key.value)
  //     val (remainders, result) = json.collectFirstWithRemainders { case (`name`, json) => json }
  //     result
  //       .toValid(Violations.rootNec(Violation.tpe(name = "value", actual = "null")))
  //       .andThen(apply(codec = codec.value, _))
  //       .leftMap(name /: _)
  //       .tupleLeft(remainders)
  //   case Field.Optional(self) =>
  //     val key = self.key
  //     val name = JsonKeyPrinter(codec = key.self.value, key.value)

  //     if json.exists((key, _) => key === name)
  //     then apply(codec = self, json).map(_.map(_.some))
  //     else (json, none).valid

  // def apply[A](codec: Tuple[Json, A], json: CirceJson): Validated[Violations, A] =
  //   json.asArray
  //     .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = toType(json))))
  //     .andThen: values =>
  //       val reference = codec.codecs.size.toInt
  //       val size = values.size

  //       Validated.cond(
  //         test = size >= reference,
  //         (),
  //         Violations.rootNec(
  //           Violation(constraint = Constraint.Collection.MinItems(reference), actual = size, hint = none)
  //         )
  //       ) *> Validated.cond(
  //         test = size <= reference,
  //         (),
  //         Violations.rootNec(
  //           Violation(constraint = Constraint.Collection.MaxItems(reference), actual = size, hint = none)
  //         )
  //       ) *> apply(codec, json = values, index = 0)

  // def apply[A](codec: Tuple[Json, A], json: Vector[CirceJson], index: Int): Validated[Violations, A] = codec match
  //   case Tuple.Empty(_)           => ().valid
  //   case Tuple.Modify(self, f, _) => apply(codec = self, json, index).map(f)
  //   case Tuple.Root(codec, _) =>
  //     json.headOption
  //       .toValid(Violations.rootNec(Violation.tpe(name = "value", actual = "null")))
  //       .andThen(apply(codec = codec.value, _))
  //       .leftMap(index /: _)
  //   case Tuple.Zip(left, right, _) =>
  //     val size = left.codecs.size.toInt
  //     val (x, y) = json.splitAt(size)
  //     (apply(codec = left, json = x, index), apply(codec = right, json = y, index = size)).tupled

  // // def apply[A](codec: Union[Json, A], json: CirceJson): Validated[Violations, A] = codec match
  // //   case codec: Union.Untagged[Json, A] => apply(codec, json)
  // //   case codec: Union.Tagged[Json, A]   => apply(codec, json)

  // // def apply[A](codec: Union.Untagged[Json, A], json: CirceJson): Validated[Violations, A] = codec match
  // //   case Union.Untagged.OrElse(left, right, _) => union(left, right, json)
  // //   case Union.Untagged.Branch(name, codec, _) =>
  // //     apply(codec = codec.value, json).leftMap(name /: _)
  // //   case Union.Untagged.Modify(self, f, _) => apply(codec = self, json).map(f)

  // // def union[A, B](
  // //     left: Union.Untagged[Json, A],
  // //     right: Union.Untagged[Json, B],
  // //     json: CirceJson
  // // ): Validated[Violations, Either[A, B]] =
  // //   apply(codec = left, json).map(_.asLeft).findValid(apply(codec = right, json).map(_.asRight))

  // // def apply[A](codec: Union.Tagged[Json, A], json: CirceJson): Validated[Violations, A] = ??? // TODO ugh
