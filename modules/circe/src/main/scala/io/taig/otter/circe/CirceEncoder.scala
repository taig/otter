package io.taig.otter.circe

import cats.data.Chain
import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.taig.otter
import io.taig.otter.*
import io.circe.syntax.*
import io.taig.otter.Schema.{Coproduct, Dynamic, Enumeration, Record}

import scala.collection.immutable.VectorMap

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder:
    override def encode[A](schema: Schema[A], a: A): Json = schema match
      case schema: Schema.Collection[?, ?] => collection.encode(schema, a)
      case schema: Schema.Coproduct[?]     => coproduct.encode(schema, a).getOrElse(Json.Null)
      case schema: Schema.Dictionary[?]    => dictionary.encode(schema, a).fold(Json.Null)(Json.fromFields)
      case schema: Schema.Dynamic[?]       => dynamic.encode(schema, a).getOrElse(Json.Null)
      case schema: Schema.Product[?]       => ???
      case schema: Schema.Record[?]        => record.encode(schema, a).fold(Json.Null)(Json.fromJsonObject)
      case schema: Schema.Value[?]         => value.encode(schema, a)

  val value: Encoder[Schema.Value, Json] = new Encoder:
    override def encode[B](schema: Schema.Value[B], b: B): Json = schema match
      case schema: Schema.Enumeration[?] => enumeration.encode(schema, b)
      case schema: Schema.Primitive[?]   => primitive.encode(schema, b)

  val collection: Encoder[Schema.Collection[?, *], Json] = new Encoder:
    override def encode[B](schema: Schema.Collection[?, B], b: B): Json = schema match
      case Schema.Collection.Root(schema, _, _) =>
        Json.fromValues(b.map(b => CirceEncoder.schema.encode(schema, b)).toList)
      case Schema.Collection.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Schema.Collection.Validate(self, _, g) => encode(self, g(b))

  val coproduct: Encoder[Schema.Coproduct, Option[Json]] = new Encoder:
    override def encode[A](schema: Schema.Coproduct[A], a: A): Option[Json] = encode(schema, a, schema.discriminator)

    def encode[A](schema: Schema.Coproduct[A], a: A, discriminator: Discriminator): Option[Json] = schema match
      case Schema.Coproduct.Root(branch, _, _, _) => encode(branch, a, discriminator).some
      case Schema.Coproduct.Optional(self)        => a.flatMap(encode(self, _, discriminator))
      case Schema.Coproduct.Validate(self, _, g)  => encode(self, g(a), discriminator)
      case schema: Schema.Coproduct.OrElse[?, ?]  => encode(schema, a, discriminator)

    def encode[A, B](schema: Schema.Coproduct.OrElse[A, B], ab: A + B, discriminator: Discriminator): Option[Json] =
      ab match
        case Left(a)  => encode(schema.left, a, discriminator)
        case Right(b) => encode(schema.right, b, discriminator)

    def encode[A, B](branch: Branch[A, B], b: B, discriminator: Discriminator): Json = discriminator match {
      case Discriminator.Nested(identifier, value) =>
        Json.obj(
          identifier := CirceEncoder.value.encode(branch.key, branch.name),
          value := CirceEncoder.schema.encode(branch.value, b)
        )
      case Discriminator.Merged(identifier) => ???
      case Discriminator.Keyed              => ???
      case Discriminator.None               => ???
    }

  val dictionary: Encoder[Schema.Dictionary, Option[VectorMap[String, Json]]] = new Encoder:
    override def encode[B](schema: Schema.Dictionary[B], b: B): Option[VectorMap[String, Json]] = schema match
      case Schema.Dictionary.Root(key, value, _, _) =>
        b.map { case (k, v) =>
          StringEncoder.value.encode(key, k).orEmpty -> CirceEncoder.schema.encode(value, v)
        }.some
      case Schema.Dictionary.Optional(self)       => b.flatMap(encode(self, _))
      case Schema.Dictionary.Validate(self, _, g) => encode(self, g(b))

  val dynamic: Encoder[Schema.Dynamic, Option[Json]] = new Encoder:
    override def encode[B](schema: Schema.Dynamic[B], b: B): Option[Json] = schema match
      case Dynamic.Root(_, _)           => fromData(b).some
      case Dynamic.Optional(self)       => b.flatMap(encode(self, _))
      case Dynamic.Validate(self, _, g) => encode(self, g(b))

  val enumeration: Encoder[Schema.Enumeration, Json] = new Encoder[Schema.Enumeration, Json]:
    override def encode[B](schema: Schema.Enumeration[B], b: B): Json = schema match
      case Enumeration.Root(schema, mapping, _, _) => value.encode(schema, mapping.inj(b))
      case Enumeration.Optional(self)              => b.fold(Json.Null)(encode(self, _))
      case Enumeration.Validate(self, _, g)        => encode(self, g(b))

  val primitive: Encoder[Schema.Primitive, Json] = new Encoder:
    override def encode[B](schema: Schema.Primitive[B], b: B): Json = schema match
      case Schema.Primitive.Root(tpe, _, _, _)   => encode(tpe, b)
      case Schema.Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))
      case Schema.Primitive.Validate(self, _, g) => encode(self, g(b))

    def encode[B](tpe: Type[B], b: B): Json = tpe match
      case Type.BigDecimal => (b: BigDecimal).asJson
      case Type.BigInt     => (b: BigInt).asJson
      case Type.Boolean    => (b: Boolean).asJson
      case Type.Double     => (b: Double).asJson
      case Type.Float      => (b: Float).asJson
      case Type.Int        => (b: Int).asJson
      case Type.Long       => (b: Long).asJson
      case Type.String     => (b: String).asJson

  val record: Encoder[Schema.Record, Option[JsonObject]] = new Encoder[Schema.Record, Option[JsonObject]]:
    override def encode[B](schema: Schema.Record[B], b: B): Option[JsonObject] =
      encode(schema, b, schema.nulls)

    def encode[B](schema: Schema.Record[B], b: B, nulls: Null): Option[JsonObject] = schema match
      case Record.Empty(_, _, _)       => JsonObject.empty.some
      case Record.Root(field, _, _, _) => encode(field, b, nulls).some
      case Record.Zip(left, right, _, _, _) =>
        (encode(left, b._1, nulls), encode(right, b._2, nulls)) match {
          case (Some(left), Some(right)) => JsonObject.fromIterable(left.toIterable ++ right.toIterable).some
          case (left @ Some(_), None)    => left
          case (None, right @ Some(_))   => right
          case (None, None)              => None
        }
      case Record.Optional(self)       => b.flatMap(encode(self, _, nulls))
      case Record.Validate(self, _, g) => encode(self, g(b), nulls)

    def encode[A, B](field: Field[A, B], b: B, nulls: Null): JsonObject =
      val hideNull = (nulls, field.nulls) match
        case (Null.Hide, None) | (_, Some(Null.Hide)) => true
        case _                                        => false

      val value = CirceEncoder.schema.encode(field.value, b)

      if value.isNull && hideNull
      then JsonObject.empty
      else JsonObject.singleton(StringEncoder.value.encode(field.key, field.name).orEmpty, value)
