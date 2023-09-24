package io.taig.otter.circe

import cats.data.{Chain, Ior, Validated}
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Schema.{Collection, Coproduct, Dynamic, Primitive, Record}
import io.taig.otter.http.syntax.*
import io.taig.otter.validation.{Violation, Violations}
import io.taig.otter.*

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder:
    override def decode[B](schema: Schema[B], json: Json): Validated[Violations, B] = schema match
      case schema: Schema.Value[?] => value.decode(schema, json)
      case schema: Schema.Collection[?, ?] =>
        if json.isNull
        then collection.decode(schema, none)
        else
          json.asArray
            .toValid(Violations.rootNec(Violation.tpe("array", actual = name(json))))
            .andThen(array => collection.decode(schema, Chain.fromSeq(array).some))
      case schema: Schema.Coproduct[?]  => coproduct.decode(schema, json)
      case schema: Schema.Dictionary[?] => ???
      case schema: Schema.Dynamic[?]    => dynamic.decode(schema, json)
      case schema: Schema.Product[?]    => ???
      case schema: Schema.Record[?] =>
        if json.isNull
        then record.decode(schema, none)
        else
          json.asObject
            .toValid(Violations.rootNec(Violation.tpe("object", actual = name(json))))
            .andThen(obj => record.decode(schema, Chain.fromIterableOnce(obj.toIterable).some))

  val value: Decoder[Schema.Value, Json] = new Decoder:
    override def decode[A](schema: Schema.Value[A], json: Json): Validated[Violations, A] = schema match
      case schema: Schema.Enumeration[?] => ???
      case schema: Schema.Primitive[?]   => primitive.decode(schema, json)

  val collection: Decoder[Schema.Collection[?, *], Option[Chain[Json]]] = new Decoder:
    override def decode[A](schema: Schema.Collection[?, A], json: Option[Chain[Json]]): Validated[Violations, A] =
      (schema, json) match
        case (Schema.Collection.Optional(self), Some(json)) => decode(self, json).map(_.some)
        case (Schema.Collection.Optional(_), None)          => none.valid[Violations]
        case (schema, Some(json))                           => decode(schema, json)
        case (_, None)                                      => Violations.rootNec(Violation.required).invalid

    def decode[A](schema: Schema.Collection[?, A], json: Chain[Json]): Validated[Violations, A] = schema match
      case Collection.Root(schema, _, _) =>
        json.zipWithIndex.traverse { case (json, index) =>
          CirceDecoder.schema.decode(schema, json).leftMap(_.modifyHistory(index /: _))
        }
      case Collection.Optional(self) => decode(self, json).map(_.some)
      case Collection.Validate(self, validation, _) =>
        decode(self, json).andThen(validation(_).leftMap(Violations.root))

  val coproduct: Decoder[Schema.Coproduct, Json] = new Decoder:
    override def decode[B](schema: Schema.Coproduct[B], json: Json): Validated[Violations, B] =
      decode(schema, json, schema.discriminator) match
        case Ior.Left(violations) => violations.invalid
        case Ior.Right(Some(b))   => b.valid
        case Ior.Right(None)      =>
          // validation error suckers
          ???
        case Ior.Both(violations, b) => b.toValid(violations)

    def decode[B](schema: Schema.Coproduct[B], json: Json, discriminator: Discriminator): Ior[Violations, Option[B]] =
      schema match
        case Coproduct.Root(branch, _, _, _) => decode(branch, json, discriminator)
        case Coproduct.Optional(self)        => decode(self, json, discriminator).map(_.some)
        case Coproduct.Validate(self, validation, _) =>
          decode(self, json, discriminator).flatMap(_.traverse(validation(_).leftMap(Violations.root).toIor))
        case schema: Coproduct.OrElse[a, b] => decode[a, b](schema, json, discriminator)

    def decode[A, B](
        schema: Schema.Coproduct.OrElse[A, B],
        json: Json,
        discriminator: Discriminator
    ): Ior[Violations, Option[A + B]] = decode(schema.left, json, discriminator) match
      case Ior.Left(violations) =>
        decode(schema.right, json, discriminator).map(_.map(_.asRight)).leftMap(violations |+| _)
      case Ior.Right(Some(a))   => a.asLeft.some.rightIor
      case Ior.Both(_, Some(b)) => b.asLeft.some.rightIor

    def decode[A, B](branch: Branch[A, B], json: Json, discriminator: Discriminator): Ior[Violations, Option[B]] =
      discriminator match
        case Discriminator.Nested(identifier, value) => ???
        case Discriminator.Merged(identifier)        => ???
        case Discriminator.Keyed                     => ???
        case Discriminator.None =>
          CirceDecoder.schema.decode(branch.value, json).toIor.map(_.some)

  val dynamic: Decoder[Schema.Dynamic, Json] = new Decoder:
    override def decode[B](schema: Schema.Dynamic[B], json: Json): Validated[Violations, B] = schema match
      case Dynamic.Root(_, _)                    => toData(json).asValue.toValid(Violations.rootNec(Violation.required))
      case Dynamic.Optional(_) if json.isNull    => none.valid[Violations]
      case Dynamic.Optional(self)                => decode(self, json).map(_.some)
      case Dynamic.Validate(self, validation, _) => decode(self, json).andThen(validation(_).leftMap(Violations.root))

  val primitive: Decoder[Schema.Primitive, Json] = new Decoder:
    override def decode[B](schema: Schema.Primitive[B], json: Json): Validated[Violations, B] = schema match
      case Primitive.Root(tpe, _, _, _) =>
        decode(tpe, json).toValid(Violations.rootNec(Violation.tpe(tpe.name, actual = name(json))))
      case Primitive.Optional(_) if json.isNull    => none.valid[Violations]
      case Primitive.Optional(self)                => decode(self, json).map(_.some)
      case Primitive.Validate(self, validation, _) => decode(self, json).andThen(validation(_).leftMap(Violations.root))

    def decode[A](tpe: Type[A], json: Json): Option[A] = tpe match
      case Type.BigDecimal => json.as[BigDecimal].toOption
      case Type.BigInt     => json.as[BigInt].toOption
      case Type.Boolean    => json.as[Boolean].toOption
      case Type.Double     => json.as[Double].toOption
      case Type.Float      => json.as[Float].toOption
      case Type.Int        => json.as[Int].toOption
      case Type.Long       => json.as[Long].toOption
      case Type.String     => json.as[String].toOption

  val record: Decoder[Schema.Record, Option[Chain[(String, Json)]]] = new Decoder.WithRemainders:
    override def decode[A](schema: Schema.Record[A], json: Option[Chain[(String, Json)]]): Validated[Violations, A] =
      // TODO allow to fail on unknown object properties
      decodeWithRemainders(schema, json).map(_._2)

    override def decodeWithRemainders[B](
        schema: Schema.Record[B],
        json: Option[Chain[(String, Json)]]
    ): Validated[Violations, (Option[Chain[(String, Json)]], B)] = (schema, json) match
      case (Schema.Record.Optional(self), Some(json)) => decodeWithRemainders(self, json).map(_.bimap(_.some, _.some))
      case (Schema.Record.Optional(_), None)          => (json, none).valid
      case (schema, Some(json))                       => decodeWithRemainders(schema, json).map(_.leftMap(_.some))
      case (_, None)                                  => Violations.rootNec(Violation.required).invalid

    def decodeWithRemainders[B](
        schema: Schema.Record[B],
        json: Chain[(String, Json)]
    ): Validated[Violations, (Chain[(String, Json)], B)] =
      schema match
        case Record.Empty(_, _, _)       => (json, ()).valid
        case Record.Root(field, _, _, _) => decodeWithRemainders(field, json)
        case Record.Zip(left, right, _, _, _) =>
          decodeWithRemainders(left, json) match
            case Validated.Valid((json, a)) =>
              decodeWithRemainders(right, json) match
                case Validated.Valid((json, b))    => (json, (a, b)).valid
                case Validated.Invalid(violations) => violations.invalid
            case Validated.Invalid(left) =>
              decodeWithRemainders(right, json) match
                case Validated.Valid(_)       => left.invalid
                case Validated.Invalid(right) => (left |+| right).invalid
        case Record.Optional(self) =>
          decodeWithRemainders(self, json).map(_.map(_.some))
        case Record.Validate(self, validation, _) =>
          decodeWithRemainders(self, json).andThen(_.traverse(validation(_).leftMap(Violations.root)))

    def decodeWithRemainders[A, B](
        field: Field[A, B],
        json: Chain[(String, Json)]
    ): Validated[Violations, (Chain[(String, Json)], B)] =
      val key = StringEncoder.value.encode(field.key, field.name).orEmpty
      val (head, tail) = json.firstWithRemainders(key).getOrElse((Json.Null, json))
      CirceDecoder.schema.decode(field.value, head).tupleLeft(tail).leftMap(_.modifyHistory(key /: _))
