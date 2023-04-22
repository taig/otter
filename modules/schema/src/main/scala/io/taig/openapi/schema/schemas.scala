package io.taig.openapi.schema

import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.implicits.*
import cats.{Eq, Eval, Order}
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.schema.syntax.*
import io.taig.validation.*

import java.util.UUID
import scala.collection.immutable.SortedMap
import scala.deriving.Mirror
import scala.reflect.Enum

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Primitive[Double] = Primitive(Type.Double).withFormat("double")
  val int: Primitive[Int] = Primitive(Type.Int).withFormat("int32")
  val float: Primitive[Float] = Primitive(Type.Float).withFormat("float")
  val long: Primitive[Long] = Primitive(Type.Long).withFormat("int64")
  val string: Primitive[String] = Primitive(Type.String)
  val uuid: Primitive[UUID] =
    string.ivalidate(validations.parser.uuid.mapReference(OpenApi.fromString))(_.toString).withFormat("uuid")

  object collection:
    def chain[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[Chain[A], B] = Collection(Eval.later(schema))
    def nonEmptyChain[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[NonEmptyChain[A], B] =
      chain(schema).ivalidate(validations.collection.chain.nonEmpty[A].map { case (head, tail) =>
        NonEmptyChain.fromChainPrepend(head, tail)
      })(_.toChain)
    def seq[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[Seq[A], B] =
      chain(schema).imap(_.toList: Seq[A])(Chain.fromSeq)
    def list[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[List[A], B] =
      chain(schema).imap(_.toList)(Chain.fromSeq)
    def vector[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[Vector[A], B] =
      chain(schema).imap(_.toVector)(Chain.fromSeq)
    def set[A, B <: OpenApi](schema: => Schema.Of[A, B]): Collection.Of[Set[A], B] = seq(schema).imap(_.toSet)(_.toSeq)
    def map[A, B, C <: OpenApi](schema: => Schema.Of[(A, B), C]): Collection.Of[Map[A, B], C] =
      seq(schema).imap(_.toMap)(_.toSeq)

  object dictionary:
    def map[A, B](key: => Schema.Of[A, OpenApi.Primitive], schema: => Schema[B]): Dictionary[Map[A, B]] =
      Dictionary(Eval.later(key), Eval.later(schema))
    def map[A](schema: => Schema[A]): Dictionary[Map[String, A]] = Dictionary(Eval.now(string), Eval.later(schema))

    def sortedMap[A: Order, B](
        key: => Schema.Of[A, OpenApi.Primitive],
        schema: => Schema[B]
    ): Dictionary[SortedMap[A, B]] = map(key, schema).imap(SortedMap.from(_))(_.toMap)
    def sortedMap[A](schema: => Schema[A]): Dictionary[SortedMap[String, A]] =
      map(schema).imap(SortedMap.from(_))(_.toMap)

    def nonEmptyMap[A: Order, B](
        key: => Schema.Of[A, OpenApi.Primitive],
        schema: => Schema[B]
    ): Dictionary[NonEmptyMap[A, B]] = sortedMap(key, schema).ivalidate {
      validations.map.sorted.nonEmpty[A, B].map { case (head, tail) => NonEmptyMap(head, SortedMap.from(tail)) }
    }(_.toSortedMap)
    def nonEmptyMap[A](schema: => Schema[A]): Dictionary[NonEmptyMap[String, A]] = sortedMap(schema).ivalidate {
      validations.map.sorted.nonEmpty[String, A].map { case (head, tail) => NonEmptyMap(head, SortedMap.from(tail)) }
    }(_.toSortedMap)

  inline def enumeration[A, B](schema: => Schema.Of[A, OpenApi.Primitive])(mapping: B => A)(using
      values: EnumValues.Aux[B, B]
  ): Enumeration[B] = Enumeration[A, B](Eval.later(schema), values.toSet, mapping)

  object product:
    val empty: Product[Unit] = Product.Empty
    def one[A](field: Field[A]): Product[A] = Product.one(field)

  object dynamic:
    val any: Dynamic[OpenApi] = Dynamic("OpenApi")(_.some)
    val value: Dynamic[OpenApi.Value] = Dynamic("OpenApi.Value")(_.asValue)
    val primitive: Dynamic[OpenApi.Primitive] = Dynamic("OpenApi.Primitive")(_.asPrimitive)
    val number: Dynamic[OpenApi.Number] = Dynamic("OpenApi.Number")(_.asNumber)
    val decimal: Dynamic[OpenApi.Decimal] = Dynamic("OpenApi.Decimal")(_.asDecimal)
    val integer: Dynamic[OpenApi.Integer] = Dynamic("OpenApi.Integer")(_.asInteger)
    val obj: Dynamic[OpenApi.Object] = Dynamic("OpenApi.Object")(_.asObject)
    val array: Dynamic[OpenApi.Array[OpenApi]] = Dynamic("OpenApi.Array")(_.asArray)
    val nil: Dynamic[OpenApi.Null.type] = Dynamic("OpenApi.Null")(openapi => Option.when(openapi.isNull)(OpenApi.Null))

  def singleton[A <: Singleton](a: A): Dynamic[A] = dynamic.nil.imap[A](_ => a)(_ => OpenApi.Null)

  def field[A](name: String, schema: => Schema[A]): Field[A] = Field(name, Eval.later(schema))
  def branch[A: Eq, B](name: A, key: => Schema.Of[A, OpenApi.Primitive], schema: => Schema[B]): Branch[A, B] =
    Branch(name, Eval.later(key), Eval.later(schema))
  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] =
    Branch(name, Eval.now(string), Eval.later(schema))

  val constraint: Sum[String, Constraint[OpenApi]] =
    val tpe: Sum[String, Constraint.Type[OpenApi]] =
      val numeric: Product[Constraint.Type.Numeric[OpenApi]] =
        (field("equal", boolean) :* field("delta", dynamic.any.optional)).gimap
      (branch("universal", singleton(Constraint.Type.Universal)) + branch("numeric", numeric)).gimap

    val or: Product[Constraint.Or[OpenApi, OpenApi]] =
      (field("left", collection.chain(constraint)) :* field("right", collection.chain(constraint))).gimap

    val rule: Product[Constraint.Rule[OpenApi]] =
      val identifier: Primitive[Constraint.Identifier] = string.imap(Constraint.Identifier.apply)(_.toString)
      (field("identifier", identifier) :* field("reference", dynamic.any.optional) :* field("type", tpe)).gimap

    (branch("or", or) + branch("rule", rule)).withoutDiscriminator.gimap

  val violation: Product[Violation[OpenApi, OpenApi]] =
    (field("constraint", constraint) :* field("actual", dynamic.any)).gimap

  val history: Primitive[History] =
    string.ivalidate(
      validations
        .parser("History")(History.parse(_).toOption)
        .mapReference(OpenApi.fromString)
    )(_.toJsonPath)

  val violations: Dictionary[Violations] =
    dictionary.nonEmptyMap(history, collection.nonEmptyChain(violation)).imap(Violations.apply)(_.toNem)
