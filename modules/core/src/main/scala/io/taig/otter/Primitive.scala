package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Ordering.Implicits.*

sealed abstract class Primitive[A] extends Codec[Data.Primitive, A]:
  self =>

  def constraints: Vector[Constraint.Primitive]

  final override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = new Primitive[A]:
    export self.{constraints, decode, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def imap[B](f: A => B)(g: B => A): Primitive[B] = new Primitive[B]:
    export self.{constraints, metadata}
    override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
    override def encode(b: B): Data.Primitive = self.encode(g(b))

  final override def to[B](using convert: Convert[A, B]): Primitive[B] = imap(convert.to)(convert.from)

object Primitive:
  final private case class Number[A <: Double | Int | Float | Long | JBigDecimal | JBigInteger](
      name: JString,
      minimum: Option[Comparison[A]],
      maximum: Option[Comparison[A]],
      multiple: Option[A],
      gteq: (A, A) => Boolean,
      gt: (A, A) => Boolean,
      lteq: (A, A) => Boolean,
      lt: (A, A) => Boolean,
      modulo0: (A, A) => Boolean,
      lift: Data.Number => Option[A],
      parse: JString => Option[A]
  ) extends Primitive[A]:
    override def constraints: Vector[Constraint.Primitive] =
      minimum.map(_.map(encode)).map(Constraint.Primitive.Minimum.apply).toVector ++
        maximum.map(_.map(encode)).map(Constraint.Primitive.Maximum.apply).toVector ++
        multiple.map(encode).map(Constraint.Primitive.Multiple.apply).toVector
    override def metadata: Metadata = Metadata.Empty

    def verifyMinimum(value: A): Codec.Result[Unit] = minimum.traverse_ {
      case comparison @ Comparison(reference, exclusive) =>
        Validated.cond(
          if exclusive then gt(value, reference) else gteq(value, reference),
          (),
          Violations.rootNec(
            Violation(Constraint.Primitive.Minimum(comparison.map(Data.Number.apply)), actual = Data.Number(value))
          )
        )
    }

    def verifyMaximum(value: A): Codec.Result[Unit] = maximum.traverse_ {
      case comparison @ Comparison(reference, exclusive) =>
        Validated.cond(
          if exclusive then lt(value, reference) else lteq(value, reference),
          (),
          Violations.rootNec(
            Violation(Constraint.Primitive.Maximum(comparison.map(Data.Number.apply)), actual = Data.Number(value))
          )
        )
    }

    def verifyMultiple(value: A): Codec.Result[Unit] = multiple.traverse_ { reference =>
      Validated.cond(
        modulo0(value, reference),
        (),
        Violations.rootNec(Violation(Constraint.Primitive.Multiple(Data.Number(value)), actual = Data.Number(value)))
      )
    }

    override def decode(data: Data): Codec.Result[A] = data.asPrimitive
      .flatMap: primitive =>
        primitive.asNumber.flatMap(lift).orElse(primitive.asString.map(_.value).flatMap(parse))
      .toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
      .andThen(value => (verifyMinimum(value) *> verifyMaximum(value) *> verifyMultiple(value)).as(value))

    override def encode(a: A): Data.Number = Data.Number(a)

  final private case class String(
      minLength: Option[Int],
      maxLength: Option[Int],
      matches: Option[Pattern]
  ) extends Primitive[JString]:
    override def constraints: Vector[Constraint.Primitive] =
      minLength.map(Constraint.Primitive.MinLength.apply).toVector ++
        maxLength.map(Constraint.Primitive.MaxLength.apply).toVector ++
        matches.map(Constraint.Primitive.Matches.apply).toVector
    override def metadata: Metadata = Metadata.Empty

    def verifyMinLength(value: JString): Codec.Result[Unit] = minLength.traverse_ { reference =>
      val length = value.length
      Validated.cond(
        length >= reference,
        (),
        Violations.rootNec(Violation(Constraint.Primitive.MinLength(reference), actual = Data.Number(length)))
      )
    }

    def verifyMaxLength(value: JString): Codec.Result[Unit] = maxLength.traverse_ { reference =>
      val length = value.length
      Validated.cond(
        length <= reference,
        (),
        Violations.rootNec(Violation(Constraint.Primitive.MaxLength(reference), actual = Data.Number(length)))
      )
    }

    def verifyMatches(value: JString): Codec.Result[Unit] = matches.traverse_ { pattern =>
      Validated.cond(
        pattern.matcher(value).matches(),
        (),
        Violations.rootNec(Violation(Constraint.Primitive.Matches(pattern), actual = Data.String(value)))
      )
    }

    override def decode(data: Data): Codec.Result[JString] = data match
      case Data.String(value) =>
        (verifyMinLength(value) *> verifyMaxLength(value) *> verifyMatches(value)).as(value)
      case _ => Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
    override def encode(a: JString): Data.Primitive = Data.String(a)

  final private case class Parser[A](
      name: JString,
      minLength: Option[Int],
      maxLength: Option[Int],
      matches: Option[Pattern],
      f: JString => Option[A],
      g: A => JString
  ) extends Primitive[A]:
    val codec = string(minLength, maxLength, matches)
    override def constraints: Vector[Constraint.Primitive] = codec.constraints
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[A] = codec
      .decode(data)
      .andThen: value =>
        f(value).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(value))))
    override def encode(a: A): Data.Primitive = codec.encode(g(a))

  case object Boolean extends Primitive[SBoolean]:
    override def constraints: Vector[Constraint.Primitive] = Vector.empty
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Codec.Result[SBoolean] = data.asPrimitive
      .flatMap: primitive =>
        primitive.asBoolean.map(_.value).orElse(primitive.asString.flatMap(_.value.toBooleanOption))
      .toValid(Violations.rootNec(Violation(Constraint.Type("boolean"), actual = Data.String(data.name))))
    override def encode(a: SBoolean): Data.Primitive = Data.Boolean(a)

  def jBigDecimal(
      minimum: Option[Comparison[JBigDecimal]],
      maximum: Option[Comparison[JBigDecimal]],
      multiple: Option[JBigDecimal]
  ): Primitive[JBigDecimal] = Number(
    name = "bigDecimal",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _.remainder(_).compareTo(JBigDecimal.ZERO) == 0,
    lift = _.toBigDecimal,
    parse = value =>
      try Some(new JBigDecimal(value))
      catch case _: NumberFormatException => None
  )

  def jBigInteger(
      minimum: Option[Comparison[JBigInteger]],
      maximum: Option[Comparison[JBigInteger]],
      multiple: Option[JBigInteger]
  ): Primitive[JBigInteger] = Number(
    name = "bigInteger",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _.mod(_) == 0,
    lift = _.toBigInteger,
    parse = value =>
      try Some(new JBigInteger(value))
      catch case _: NumberFormatException => None
  )

  def double(
      minimum: Option[Comparison[Double]],
      maximum: Option[Comparison[Double]],
      multiple: Option[Double]
  ): Primitive[Double] = Number(
    name = "double",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _ % _ == 0,
    lift = _.toDouble,
    parse = _.toDoubleOption
  )

  def float(
      minimum: Option[Comparison[Float]],
      maximum: Option[Comparison[Float]],
      multiple: Option[Float]
  ): Primitive[Float] = Number(
    name = "float",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _ % _ == 0,
    lift = _.toFloat,
    parse = _.toFloatOption
  )

  def int(
      minimum: Option[Comparison[Int]],
      maximum: Option[Comparison[Int]],
      multiple: Option[Int]
  ): Primitive[Int] = Number(
    name = "int",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _ % _ == 0,
    lift = _.toInt,
    parse = _.toIntOption
  )

  def long(
      minimum: Option[Comparison[Long]],
      maximum: Option[Comparison[Long]],
      multiple: Option[Long]
  ): Primitive[Long] = Number(
    name = "long",
    minimum,
    maximum,
    multiple,
    _ >= _,
    _ > _,
    _ <= _,
    _ < _,
    _ % _ == 0,
    lift = _.toLong,
    parse = _.toLongOption
  )

  def string(
      minLength: Option[Int],
      maxLength: Option[Int],
      matches: Option[Pattern]
  ): Primitive[JString] = String(minLength, maxLength, matches)

  def parser[A](
      name: JString,
      minLength: Option[Int],
      maxLength: Option[Int],
      matches: Option[Pattern],
      f: JString => Option[A],
      g: A => JString
  ): Primitive[A] = Parser(name, minLength, maxLength, matches, f, g)

  val boolean: Primitive[SBoolean] = Boolean

  given CodecInvariant[Primitive] with
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)

  given [A]: Metadata.Ops[Primitive[A]] with
    extension (self: Primitive[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Primitive[A] = self.modifyMetadata(f)
