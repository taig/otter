package io.taig.otter.schema

import cats.Hash
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.implicits.*
import io.taig.enumeration.ext.{EnumerationValues, Mapping}
import io.taig.otter.OpenApi
import io.taig.otter.validation.{validations, Constraint, Validation, Violation}
import org.typelevel.ci.CIString

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.{SortedMap, SortedSet, VectorMap}

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Primitive(Type.Boolean)
  val double: Primitive[Double] = Primitive(Type.Double).format("double")
  val int: Primitive[Int] = Primitive(Type.Int).format("int32")
  val float: Primitive[Float] = Primitive(Type.Float).format("float")
  val long: Primitive[Long] = Primitive(Type.Long).format("int64")
  val string: Primitive[String] = Primitive(Type.String)
  val nonEmptyString: Primitive[Option[String]] = string.imap(_.some.filter(_.nonEmpty))(_.orEmpty)
  val password: Primitive[String] = string.format("password")
  val uuid: Primitive[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  val date: Primitive[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  val dateTime: Primitive[LocalDateTime] = string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  val cistring: Primitive[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")

  object dynamic:
    val value: Dynamic[OpenApi.Value] = Dynamic.Value
    val any: Dynamic[OpenApi] = value.optional.imap(_.getOrElse(OpenApi.Null))(_.asValue)
    def singleton[A <: Singleton](a: A): Dynamic[A] = any.imap(_ => a)(_ => OpenApi.Null)

  def field[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Field[B] = Field(name, key, schema)
  def field[A](name: String, schema: => Schema[A]): Field[A] = field(name, string, schema)
  def field[A](name: Int, schema: => Schema[A]): Field[A] = field(name, int, schema)

  def branch[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Branch[A, B] = Branch(name, key, schema)
  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] = branch(name, string, schema)
  def branch[A](name: Int, schema: => Schema[A]): Branch[Int, A] = branch(name, int, schema)

  object collection:
    def chain[F[a] <: Schema[a], A](schema: => F[A]): Collection[F, Chain[A]] = Collection(schema)
    def vector[F[a] <: Schema[a], A](schema: => F[A]): Collection[F, Vector[A]] =
      chain(schema).imap(_.toVector)(Chain.fromSeq)
    def list[F[a] <: Schema[a], A](schema: => F[A]): Collection[F, List[A]] =
      chain(schema).imap(_.toList)(Chain.fromSeq)
    def sortedSet[F[a] <: Schema[a], A: Ordering](schema: => F[A]): Collection[F, SortedSet[A]] =
      chain(schema).imap(values => SortedSet.from(values.iterator))(Chain.fromIterableOnce)
    def nonEmptyChain[F[a] <: Schema[a], A](schema: => F[A]): Collection[F, NonEmptyChain[A]] =
      val validation: Validation[Chain[A], NonEmptyChain[A]] =
        Validation(Constraint.MinItems(1))(NonEmptyChain.fromChain(_).toValidNec(OpenApi.Integer(0)))
      chain(schema).ivalidate(validation)(_.toChain)
    // TODO expose way to merge into record or product
    def sortedMap[F[a] <: Schema[a], A: Ordering, B](key: => Schema[A], value: => Schema[B])(
        f: (Schema[A], Schema[B]) => F[(A, B)]
    ): Collection[F, SortedMap[A, B]] =
      chain(f(key, value)).imap(values => SortedMap.from(values.iterator))(Chain.fromIterableOnce)

  def enumeration[A, B](schema: => Schema.Value[A])(using mapping: Mapping[B, A]): Enumeration[B] =
    Enumeration(schema, mapping)
  def enumeration[A: Hash, B](schema: => Schema.Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration[B] = enumeration(schema)(using Mapping.enumeration(f))

  object dictionary:
    def vectorMap[A, B](key: => Schema.Value[A], schema: => Schema[B]): Dictionary[VectorMap[A, B]] =
      Dictionary(key, schema)
    def sortedMap[A: Ordering, B](key: => Schema.Value[A], schema: => Schema[B]): Dictionary[SortedMap[A, B]] =
      vectorMap(key, schema).imap(SortedMap.from)(_.to(VectorMap))
    def nonEmptyMap[A: Ordering, B](key: => Schema.Value[A], schema: => Schema[B]): Dictionary[NonEmptyMap[A, B]] =
      val validation: Validation[SortedMap[A, B], NonEmptyMap[A, B]] =
        Validation(Constraint.MinProperties(1))(NonEmptyMap.fromMap(_).toValidNec(OpenApi.Integer(0)))
      sortedMap(key, schema).ivalidate(validation)(_.toSortedMap)

  val violations: Dictionary[Violations] =
    val pattern: Primitive[Pattern] = string.imap(Pattern.compile)(_.pattern)

    val constraint: Schema[Constraint] = (
      branch("equals", field("reference", string).to[Constraint.Equals]) :+
        branch("minLength", field("reference", int).to[Constraint.MinLength]) :+
        branch("maxLength", field("reference", int).to[Constraint.MaxLength]) :+
        branch("matches", field("pattern", pattern).to[Constraint.Matches]) :+
        branch("minimum", (field("reference", bigDecimal) :* field("exclusive", boolean)).to[Constraint.Minimum]) :+
        branch("maximum", (field("reference", bigDecimal) :* field("exclusive", boolean)).to[Constraint.Maximum]) :+
        branch("multiple", field("reference", bigDecimal).to[Constraint.Multiple]) :+
        branch("minItems", field("reference", long).to[Constraint.MinItems]) :+
        branch("maxItems", field("reference", long).to[Constraint.MaxItems]) :+
        branch("uniqueItems", dynamic.singleton(Constraint.UniqueItems)) :+
        branch("minProperties", field("reference", int).to[Constraint.MinProperties]) :+
        branch("maxProperties", field("reference", int).to[Constraint.MaxProperties]) :+
        branch("type", field("name", string).to[Constraint.Type]) :+
        branch("oneOf", field("values", collection.chain(string)).to[Constraint.OneOf]) :+
        branch("required", dynamic.singleton(Constraint.Required))
    ).to

    val violation: Record[Violation] = (field("constraint", constraint) :* field("actual", dynamic.any)).to

    val history: Primitive[History] =
      string.ivalidate(Validation.parse("history")(History.parse(_).toOption))(_.toJsonPath)

    dictionary.nonEmptyMap(history, collection.nonEmptyChain(violation)).imap(Violations.apply)(_.toNem)
