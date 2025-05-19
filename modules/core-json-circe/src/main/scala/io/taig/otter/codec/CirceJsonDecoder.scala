package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Discriminator
import io.taig.otter.Json
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.toType
import io.taig.otter.toValue

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  val collection = CollectionDecoder(decoder = this)
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = CirceJsonEncoder), render = toValue)
  val dictionary = DictionaryDecoder(key = KeyParser, value = this)
  val enumeration = EnumerationDecoder(codec = Codec(decoder = this, encoder = CirceJsonEncoder), render = toValue)
  val nullable = NullableDecoder(decoder = this, empty = _.isNull)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec, value = this, empty = CirceJson.Null)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self)
  )
  val sum = (discriminator: Discriminator) =>
    SumDecoder(
      branch = BranchDecoder(key = KeyCodec, value = this)(discriminator)
        .mapK[Json.Branch]([A] => (branch: Json.Branch[A]) => branch.self)
    )
  val tuple = TupleDecoder(decoder = this)
  val union = UnionDecoder(decoder = this)

  override def decode[A](codec: Json[A], json: CirceJson): Validated[Violations, A] = codec match
    case Json.Collection(self) =>
      json.asArray
        .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = toType(json))))
        .andThen(collection.decode(schema = self, _))
    case Json.Constant(self) => constant.decode(schema = self, json)
    case Json.Dictionary(self) =>
      json.asObject
        .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = toType(json))))
        .map(_.toList)
        .andThen(dictionary.decode(schema = self, _))
    case Json.Enumeration(self) => enumeration.decode(schema = self, json)
    case Json.Nullable(self)    => nullable.decode(schema = self, json)
    case Json.Primitive(self)   => CirceJsonPrimitiveDecoder.decode(schema = self, json)
    case Json.Record(self) =>
      json.asObject
        .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = toType(json))))
        .map(_.toList)
        .andThen(record.decode(schema = self, _))
    case Json.Sum(self) =>
      json.asObject
        .toValid(Violations.rootNec(Violation.tpe(name = "object", actual = toType(json))))
        .map(_.toList)
        .andThen(sum(self.discriminator).decode(schema = self, _))
    case Json.Tuple(self) =>
      json.asArray
        .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = toType(json))))
        .andThen(tuple.decode(schema = self, _))
    case Json.Union(self) => union.decode(schema = self, json)
