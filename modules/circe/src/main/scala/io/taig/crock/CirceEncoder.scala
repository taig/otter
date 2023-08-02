package io.taig.crock

import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import io.taig.crock.schema.*
import io.taig.crock.schema.Field.Null
import io.taig.crock.schema.Record.Null

import scala.collection.mutable.ListBuffer

object CirceEncoder:
  val schema: Encoder[Schema, Json] = new Encoder[Schema, Json]:
    override def encode[A](schema: Schema[A], b: A): Json = schema match
      case schema: Schema.Value[A] => value.encode(schema, b)
      case schema: Collection[A]   => collection.encode(schema, b)
      case schema: Record[A]       => record.encode(schema, b).fold(Json.Null)(Json.fromJsonObject)
      case schema: Product[A]      => product.encode(schema, b).fold(Json.Null)(Json.fromValues)

  val value: Encoder[Schema.Value, Json] = new Encoder[Schema.Value, Json]:
    override def encode[A](schema: Schema.Value[A], b: A): Json = schema match
      case schema: Primitive[A]   => primitive.encode(schema, b)
      case schema: Enumeration[A] => enumeration.encode(schema, b)

  val primitive: Encoder[Primitive, Json] = new Encoder[Primitive, Json]:
    def encode[A](tpe: Type[A], b: A): Json = tpe match
      case Type.BigDecimal => Json.fromBigDecimal(b)
      case Type.BigInt     => Json.fromBigInt(b)
      case Type.Boolean    => Json.fromBoolean(b)
      case Type.Double     => Json.fromDoubleOrString(b)
      case Type.Float      => Json.fromFloatOrString(b)
      case Type.Int        => Json.fromInt(b)
      case Type.Long       => Json.fromLong(b)
      case Type.String     => Json.fromString(b)

    override def encode[A](primitive: Primitive[A], b: A): Json = primitive match
      case Primitive.Root(_, tpe)         => encode(tpe, b)
      case Primitive.Validate(self, _, g) => encode(self, g(b))
      case Primitive.Optional(self)       => b.fold(Json.Null)(encode(self, _))

  val collection: Encoder[Collection, Json] = new Encoder[Collection, Json]:
    override def encode[A](collection: Collection[A], b: A): Json = collection match
      case Collection.Root(of, _)                   => Json.fromValues(b.map(CirceEncoder.schema.encode(of.value, _)))
      case collection: Collection.Validate[?, ?, ?] => encode(collection.self, collection.g(b))
      case collection: Collection.Optional[?, ?]    => b.fold(Json.Null)(encode(collection.self, _))

  val enumeration: Encoder[Enumeration, Json] = new Encoder[Enumeration, Json]:
    override def encode[A](enumeration: Enumeration[A], b: A): Json = enumeration match
      case Enumeration.Root(mapping, schema, _) => CirceEncoder.schema.encode(schema.value, mapping.inj(b))
      case Enumeration.Validate(self, _, g)     => encode(self, g(b))
      case Enumeration.Optional(self)           => b.fold(Json.Null)(encode(self, _))

  val record: Encoder[Record, Option[JsonObject]] = new Encoder:
    override def encode[A](record: Record[A], a: A): Option[JsonObject] =
      val result = ListBuffer.empty[(String, Json)]
      encode(record, a, result)
      if result.isEmpty && record.isOptional then none else JsonObject.fromIterable(result).some

    def encode[A](record: Record[A], a: A, result: ListBuffer[(String, Json)]): Unit = record match
      case Record.Empty(_)               => ()
      case Record.One(field, properties) => encodeField(field, a, properties.nulls).foreach(result.append)
      case Record.Zip(left, right, _) =>
        encode(left, a._1, result)
        encode(right, a._2, result)
      case Record.Validate(self, _, g) => encode(self, g(a), result)
      case Record.Optional(self)       => a.foreach(encode(self, _, result))

    def encodeField[A, B](field: Field[A, B], b: B, nulls: Record.Null): Option[(String, Json)] =
      val hideNull = field.nulls.value match
        case Field.Null.Hide => true
        case Field.Null.Inherit =>
          nulls match
            case Record.Null.Show => false
            case Record.Null.Hide => true
        case Field.Null.Show => false

      val value = CirceEncoder.schema.encode(field.schema.value, b)

      if hideNull && value.isNull
      then none
      else
        val key = StringEncoder.value.encode(field.key.value, field.name)
        (key.orEmpty, value).some

  val product: Encoder[Product, Option[List[Json]]] = new Encoder:
    override def encode[A](product: Product[A], b: A): Option[List[Json]] =
      val result = ListBuffer.empty[Json]
      encode(product, b, result)
      if result.isEmpty && product.isOptional then none else result.toList.some

    def encode[A](product: Product[A], b: A, result: ListBuffer[Json]): Unit = product match
      case Product.Empty(_)       => ()
      case Product.One(schema, _) => result.append(CirceEncoder.schema.encode(schema.value, b))
      case Product.Zip(left, right, _) =>
        encode(left, b._1, result)
        encode(right, b._2, result)
      case Product.Validate(self, _, g) => encode(self, g(b), result)
      case Product.Optional(self)       => b.foreach(encode(self, _, result))
