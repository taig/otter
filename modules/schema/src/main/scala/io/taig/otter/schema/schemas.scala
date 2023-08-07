package io.taig.otter.schema

import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.{Eval, Hash}
import cats.implicits.*
import io.taig.enumeration.ext.{EnumerationValues, Mapping}
import io.taig.otter.validation.{validations, Constraint, Validation, Violation}
import org.typelevel.ci.CIString

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.{ListMap, SortedMap}

object schemas:
  val bigDecimal: Primitive[BigDecimal] = Schema.Primitive(Type.BigDecimal)
  val bigInt: Primitive[BigInt] = Schema.Primitive(Type.BigInt)
  val boolean: Primitive[Boolean] = Schema.Primitive(Type.Boolean)
  val double: Primitive[Double] = Schema.Primitive(Type.Double).format("double")
  val int: Primitive[Int] = Schema.Primitive(Type.Int).format("int32")
  val float: Primitive[Float] = Schema.Primitive(Type.Float).format("float")
  val long: Primitive[Long] = Schema.Primitive(Type.Long).format("int64")
  val string: Primitive[String] = Schema.Primitive(Type.String)
  val password: Primitive[String] = string.format("password")
  val uuid: Primitive[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  val date: Primitive[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  val dateTime: Primitive[LocalDateTime] = string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  val cistring: Primitive[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")

  val void: Void[Unit] = Schema.Void.Root
  def singleton[A <: Singleton](a: A): Void[A] = void.imap(_ => a)(_ => ())

  // val anyValue: AnyValue[Any] = ??? // Schema.AnyValue.Root

  def field[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Field[A, B] =
    Field(name, Eval.later(key), Eval.later(schema))
  def field[A](name: String, schema: => Schema[A]): Field[String, A] = field(name, string, schema)
  def field[A](name: Int, schema: => Schema[A]): Field[Int, A] = field(name, int, schema)

  def branch[A, B](name: A, key: => Schema.Value[A], schema: => Schema[B]): Branch[A, B] =
    Branch(Eval.later(key), name, Eval.later(schema))
  def branch[A](name: String, schema: => Schema[A]): Branch[String, A] = branch(name, string, schema)
  def branch[A](name: Int, schema: => Schema[A]): Branch[Int, A] = branch(name, int, schema)

  object collection:
    def chain[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, Chain[A]] =
      Schema.Collection(Eval.later(schema))
    def vector[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, Vector[A]] =
      chain(schema).imap(_.toVector)(Chain.fromSeq)
    def list[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, List[A]] =
      chain(schema).imap(_.toList)(Chain.fromSeq)
    def nonEmptyChain[F[a] <: Schema[a], A](schema: => F[A]): Collection.Of[F, NonEmptyChain[A]] =
      chain(schema)
        .ivalidate(Validation(Constraint.MinItems(1))(NonEmptyChain.fromChain(_).toValidNec(none)))(_.toChain)

  def enumeration[A, B](schema: => Value[A])(using mapping: Mapping[B, A]): Enumeration[B] =
    Schema.Enumeration(Eval.later(schema), mapping)
  def enumeration[A: Hash, B](schema: => Value[A])(f: B => A)(using
      EnumerationValues.Aux[B, B]
  ): Enumeration[B] = enumeration(schema)(using Mapping.enumeration(f))

  object dictionary:
    def listMap[A, B](key: => Value[A], schema: => Schema[B]): Dictionary[ListMap[A, B]] =
      Schema.Dictionary(Eval.later(key), Eval.later(schema))
    def sortedMap[A: Ordering, B](key: => Value[A], schema: => Schema[B]): Dictionary[SortedMap[A, B]] =
      listMap(key, schema).imap(SortedMap.from)(_.to(ListMap))
    def nonEmptyMap[A: Ordering, B](key: => Value[A], schema: => Schema[B]): Dictionary[NonEmptyMap[A, B]] =
      sortedMap(key, schema)
        .ivalidate(Validation(Constraint.MinProperties(1))(NonEmptyMap.fromMap(_).toValidNec(none)))(_.toSortedMap)

  val violations: Schema[Violations] =
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
        branch("uniqueItems", singleton(Constraint.UniqueItems)) :+
        branch("minProperties", field("reference", int).to[Constraint.MinProperties]) :+
        branch("maxProperties", field("reference", int).to[Constraint.MaxProperties]) :+
        branch("type", field("name", string).to[Constraint.Type]) :+
        branch("oneOf", field("values", collection.list(string)).to[Constraint.OneOf]) :+
        branch("required", singleton(Constraint.Required))
    ).to

    val violation: Record[Violation] = (field("constraint", constraint) :* field("actual", string.optional)).to

    val history: Primitive[History] =
      string.ivalidate(Validation.parse("history")(History.parse(_).toOption))(_.toJsonPath)

    dictionary.nonEmptyMap(history, collection.nonEmptyChain(violation)).imap(Violations.apply)(_.toNem)
