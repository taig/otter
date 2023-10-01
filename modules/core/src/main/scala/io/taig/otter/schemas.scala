package io.taig.otter

import cats.Hash
import cats.Eq
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.implicits.*
import io.taig.enumeration.ext.{EnumerationValues, Mapping}
import io.taig.otter.validation.*
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
    val value: Dynamic[Data.Value] = Dynamic.Default
    val any: Dynamic[Data] = value.optional.imap(_.getOrElse(Data.Null))(_.asValue)
    val empty: Dynamic[Data.Null.type] =
      val validation: Validation[Data, Data.Null.type] = Validation(Constraint.Type("null")):
        case Data.Null => Data.Null.valid
        case data      => Data.String(data.name).invalidNec
      any.ivalidate(validation)(identity)
    def singleton[A <: Singleton](a: A): Dynamic[A] = empty.imap(_ => a)(_ => Data.Null)
    val primitive: Dynamic.Primitive[Data.Primitive] = Dynamic.Primitive.Default
    val number: Dynamic.Primitive[Data.Number] =
      val validation: Validation[Data.Primitive, Data.Number] = Validation(Constraint.Type("number")):
        case data: Data.Number => data.valid
        case data              => Data.String(data.name).invalidNec
      primitive.ivalidate(validation)(identity)

  def field[A, B](name: A, key: Schema.Value[A], schema: Schema[B]): Field[B] = Field(name, key, schema)
  def field[A](name: String, schema: Schema[A]): Field[A] = field(name, string, schema)
  def field[A](name: Int, schema: Schema[A]): Field[A] = field(name, int, schema)

  def branch[A: Eq, B](name: A, key: Schema.Value[A], schema: Schema[B]): Branch[B] = Branch(name, key, schema)
  def branch[A](name: String, schema: Schema[A]): Branch[A] = branch(name, string, schema)
  def branch[A](name: Int, schema: Schema[A]): Branch[A] = branch(name, int, schema)

  object collection:
    def chain[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F[A], Chain[A]] = Collection(schema)
    def vector[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F[A], Vector[A]] =
      chain(schema).imap(_.toVector)(Chain.fromSeq)
    def list[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F[A], List[A]] =
      chain(schema).imap(_.toList)(Chain.fromSeq)
    def sortedSet[F[a] <: Schema[a], A: Ordering](schema: F[A]): Collection.Of[F[A], SortedSet[A]] =
      chain(schema).imap(values => SortedSet.from(values.iterator))(Chain.fromIterableOnce)
    def nonEmptyChain[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F[A], NonEmptyChain[A]] =
      val validation: Validation[Chain[A], NonEmptyChain[A]] =
        Validation(Constraint.MinItems(1))(NonEmptyChain.fromChain(_).toValidNec(Data.Number(0)))
      chain(schema).ivalidate(validation)(_.toChain)
    def sortedMap[F[a] <: Schema[a], A: Ordering, B](key: Schema[A], schema: Schema[B])(
        f: (Schema[A], Schema[B]) => F[(A, B)]
    ): Collection.Of[F[(A, B)], SortedMap[A, B]] =
      chain(f(key, schema)).imap(values => SortedMap.from(values.iterator))(Chain.fromIterableOnce)

  object enumeration:
    def apply[A, B](schema: Schema.Value[A])(using mapping: Mapping[B, A]): Enumeration[B] =
      Enumeration(schema, mapping)
    def apply[A: Hash, B](schema: Schema.Value[A])(f: B => A)(using EnumerationValues.Aux[B, B]): Enumeration[B] =
      enumeration(schema)(using Mapping.enumeration(f))
    def constant[A: Eq](schema: Schema.Value[A], value: A & Singleton): Enumeration[value.type] =
      enumeration(schema)(using Mapping.constant[A](value))

  object dictionary:
    def chain[A, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[Chain[(A, B)]] = Dictionary(key, schema)
    def map[A, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[Map[A, B]] =
      chain(key, schema).imap(values => Map.from(values.iterator))(Chain.fromIterableOnce)
    def vectorMap[A, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[VectorMap[A, B]] =
      chain(key, schema).imap(values => VectorMap.from(values.iterator))(Chain.fromIterableOnce)
    def sortedMap[A: Ordering, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[SortedMap[A, B]] =
      chain(key, schema).imap(values => SortedMap.from(values.iterator))(Chain.fromIterableOnce)
    def nonEmptyMap[A: Ordering, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[NonEmptyMap[A, B]] =
      val validation: Validation[SortedMap[A, B], NonEmptyMap[A, B]] =
        Validation(Constraint.MinProperties(1))(NonEmptyMap.fromMap(_).toValidNec(Data.Number(0)))
      sortedMap(key, schema).ivalidate(validation)(_.toSortedMap)

  val violations: Dictionary[Violations] =
    val pattern: Primitive[Pattern] = string.imap(Pattern.compile)(_.pattern)

    val constraint: Coproduct[Constraint] = (
      branch("equals", field("reference", string).to[Constraint.Equals]) :+
        branch("minLength", field("reference", int).to[Constraint.MinLength]) :+
        branch("maxLength", field("reference", int).to[Constraint.MaxLength]) :+
        branch("matches", field("pattern", pattern).to[Constraint.Matches]) :+
        branch("minimum", (field("reference", dynamic.number) :* field("exclusive", boolean)).to[Constraint.Minimum]) :+
        branch("maximum", (field("reference", dynamic.number) :* field("exclusive", boolean)).to[Constraint.Maximum]) :+
        branch("multiple", field("reference", dynamic.number).to[Constraint.Multiple]) :+
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
      string.ivalidate(validations.parse("history")(History.parse(_).toOption))(_.toJsonPath)

    dictionary.nonEmptyMap(history, collection.nonEmptyChain(violation)).imap(Violations.apply)(_.toNem)

  def error[A](identifier: String, value: Schema[A]): Coproduct[A] = branch(identifier, value).toCoproduct
