package io.taig.otter

import cats.{Applicative, Eq, Hash, Id, Order, Traverse}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet}
import cats.implicits.*
import io.taig.enumeration.ext.{EnumerationValues, Mapping}
import io.taig.otter.validation.*
import org.typelevel.ci.CIString

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID
import java.util.regex.Pattern
import scala.collection.immutable.{SortedMap, SortedSet, VectorMap}

trait codecs:
  val bigDecimal: Primitive.Required[BigDecimal] = Primitive.Required(Type.BigDecimal)
  val bigInt: Primitive.Required[BigInt] = Primitive.Required(Type.BigInt)
  val boolean: Primitive.Required[Boolean] = Primitive.Required(Type.Boolean)
  val double: Primitive.Required[Double] = Primitive.Required(Type.Double).format("double")
  val int: Primitive.Required[Int] = Primitive.Required(Type.Int).format("int32")
  val float: Primitive.Required[Float] = Primitive.Required(Type.Float).format("float")
  val long: Primitive.Required[Long] = Primitive.Required(Type.Long).format("int64")
  val string: Primitive.Required[String] = Primitive.Required(Type.String)
  val nonEmptyString: Primitive.Required[Option[String]] = string.imap(_.some.filter(_.nonEmpty))(_.orEmpty)
  val password: Primitive.Required[String] = string.format("password")
  val uuid: Primitive.Required[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  val date: Primitive.Required[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  val dateTime: Primitive.Required[LocalDateTime] =
    string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  val cistring: Primitive.Required[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")
  val nonEmptyCIString: Primitive.Required[Option[CIString]] = cistring.imap(_.some.filter(_.nonEmpty))(_.orEmpty)

  object dynamic:
    val value: Dynamic[Data.Value] = Dynamic.Default
    val any: Dynamic[Data] = value.optional.imap(_.getOrElse(Data.Null))(_.asValue)
    val empty: Dynamic[Data.Null.type] =
      val validation: Validation[Data, Data.Null.type] = Validation(Constraint.Type("null")):
        case Data.Null => Data.Null.valid
        case data      => Data.String(data.name).invalidNec
      any.ivalidate(validation)(identity)
    val obj: Dynamic[Data.Object] =
      val validation: Validation[Data.Value, Data.Object] = Validation(Constraint.Type("object")):
        case data: Data.Object => data.valid
        case data              => Data.String(data.name).invalidNec
      value.ivalidate(validation)(identity)
    val primitive: Dynamic[Data.Primitive] =
      val validation: Validation[Data.Value, Data.Primitive] = Validation(Constraint.Type("number")):
        case data: Data.Primitive => data.valid
        case data                 => Data.String(data.name).invalidNec
      value.ivalidate(validation)(identity)
    val number: Dynamic[Data.Number] =
      val validation: Validation[Data.Value, Data.Number] = Validation(Constraint.Type("number")):
        case data: Data.Number => data.valid
        case data              => Data.String(data.name).invalidNec
      value.ivalidate(validation)(identity)

  def singleton[A <: Singleton](a: A): Dynamic[A] = dynamic.empty.imap(_ => a)(_ => Data.Null)

  def field[A: Eq, B](name: A, key: => Value.Required[A], codec: => Codec[B]): Field[B] = Field(name, key, codec)
  def field[A](name: String, codec: => Codec[A]): Field[A] = field(name, string, codec)
  def field[A](name: Int, codec: => Codec[A]): Field[A] = field(name, int, codec)

  def branch[A: Eq, B](name: A, key: => Value.Required[A], codec: => Codec[B]): Branch[B] = Branch(name, key, codec)
  def branch[A](name: String, codec: => Codec[A]): Branch[A] = branch(name, string, codec)
  def branch[A](name: Int, codec: => Codec[A]): Branch[A] = branch(name, int, codec)

  object collection:
    def chain[F[a] <: Codec[a], A](codec: => F[A]): Collection.Of[F[A], Chain[A]] = Collection(codec)
    def vector[F[a] <: Codec[a], A](codec: => F[A]): Collection.Of[F[A], Vector[A]] =
      chain(codec).imap(_.toVector)(Chain.fromSeq)
    def list[F[a] <: Codec[a], A](codec: => F[A]): Collection.Of[F[A], List[A]] =
      chain(codec).imap(_.toList)(Chain.fromSeq)
    def sortedSet[F[a] <: Codec[a], A: Ordering](codec: => F[A]): Collection.Of[F[A], SortedSet[A]] =
      chain(codec).imap(values => SortedSet.from(values.iterator))(Chain.fromIterableOnce)
    def nonEmptyChain[F[a] <: Codec[a], A](codec: => F[A]): Collection.Of[F[A], NonEmptyChain[A]] =
      val validation: Validation[Chain[A], NonEmptyChain[A]] =
        Validation(Constraint.MinItems(1))(NonEmptyChain.fromChain(_).toValidNec(Data.Number(0)))
      chain(codec).ivalidate(validation)(_.toChain)
    def nonEmptyList[F[a] <: Codec[a], A](codec: => F[A]): Collection.Of[F[A], NonEmptyList[A]] =
      val validation: Validation[List[A], NonEmptyList[A]] =
        Validation(Constraint.MinItems(1))(NonEmptyList.fromList(_).toValidNec(Data.Number(0)))
      list(codec).ivalidate(validation)(_.toList)
    def nonEmptySet[F[a] <: Codec[a], A: Order](codec: => F[A]): Collection.Of[F[A], NonEmptySet[A]] =
      val validation: Validation[SortedSet[A], NonEmptySet[A]] =
        Validation(Constraint.MinItems(1))(NonEmptySet.fromSet(_).toValidNec(Data.Number(0)))
      sortedSet(codec).ivalidate(validation)(_.toSortedSet)
    def sortedMap[F[a] <: Codec[a], A: Ordering, B](key: => Codec[A], codec: => Codec[B])(
        f: (Codec[A], Codec[B]) => F[(A, B)]
    ): Collection.Of[F[(A, B)], SortedMap[A, B]] =
      chain(f(key, codec)).imap(values => SortedMap.from(values.iterator))(Chain.fromIterableOnce)

  object enumeration:
    def apply[F[_], A, B](codec: => Value.Required[F[A]])(using mapping: Mapping[B, A])(using
        Applicative[F] & Traverse[F]
    ): Enumeration.Required[F[B]] =
      Enumeration.Required[F, A, B](codec, mapping)
    def apply[F[_], A: Hash, B](codec: => Value.Required[F[A]])(f: B => A)(using
        EnumerationValues.Aux[B, B]
    )(using Applicative[F] & Traverse[F]): Enumeration.Required[F[B]] = enumeration(codec)(using Mapping.enumeration(f))
    def apply[A, B](codec: => Value.Required[A])(using mapping: Mapping[B, A]): Enumeration.Required[B] =
      Enumeration.Required[Id, A, B](codec, mapping)
    def apply[A: Hash, B](codec: => Value.Required[A])(f: B => A)(using
        EnumerationValues.Aux[B, B]
    ): Enumeration.Required[B] = enumeration[Id, A, B](codec)(using Mapping.enumeration(f))
    def constant[A: Eq](codec: => Value.Required[A], value: A & Singleton): Enumeration.Required[value.type] =
      enumeration[Id, A, value.type](codec)(using Mapping.constant[A](value))

  object dictionary:
    def chain[A, B](key: => Value.Required[A], codec: => Codec[B]): Dictionary[Chain[(A, B)]] = Dictionary(key, codec)
    def map[A, B](key: => Value.Required[A], codec: => Codec[B]): Dictionary[Map[A, B]] =
      chain(key, codec).imap(values => Map.from(values.iterator))(Chain.fromIterableOnce)
    def vectorMap[A, B](key: => Value.Required[A], codec: => Codec[B]): Dictionary[VectorMap[A, B]] =
      chain(key, codec).imap(values => VectorMap.from(values.iterator))(Chain.fromIterableOnce)
    def sortedMap[A: Ordering, B](key: => Value.Required[A], codec: => Codec[B]): Dictionary[SortedMap[A, B]] =
      chain(key, codec).imap(values => SortedMap.from(values.iterator))(Chain.fromIterableOnce)
    def nonEmptyMap[A: Ordering, B](key: => Value.Required[A], codec: => Codec[B]): Dictionary[NonEmptyMap[A, B]] =
      val validation: Validation[SortedMap[A, B], NonEmptyMap[A, B]] =
        Validation(Constraint.MinProperties(1))(NonEmptyMap.fromMap(_).toValidNec(Data.Number(0)))
      sortedMap(key, codec).ivalidate(validation)(_.toSortedMap)

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
        branch("uniqueItems", singleton(Constraint.UniqueItems)) :+
        branch("minProperties", field("reference", int).to[Constraint.MinProperties]) :+
        branch("maxProperties", field("reference", int).to[Constraint.MaxProperties]) :+
        branch("type", field("name", string).to[Constraint.Type]) :+
        branch("oneOf", field("values", collection.chain(dynamic.primitive)).to[Constraint.OneOf]) :+
        branch("required", singleton(Constraint.Required))
    ).to

    val violation: Record[Violation] = (field("constraint", constraint) :* field("actual", dynamic.any)).to

    val history: Primitive.Required[History] =
      string.ivalidate(validations.parse("history")(History.parse(_).toOption))(_.toJsonPath)

    dictionary
      .nonEmptyMap(history, collection.nonEmptyChain(violation))
      .imap(Violations.apply)(_.toNem)
      .name("Violations")

  def error[A](identifier: String, codec: => Codec[A]): Coproduct[A] = branch(identifier, codec).toCoproduct

object codecs extends codecs
