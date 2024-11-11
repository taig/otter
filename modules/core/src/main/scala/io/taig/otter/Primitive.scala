// package io.taig.otter

// import cats.data.Validated
// import cats.syntax.all.*

// import java.lang.String as JString
// import java.math.BigDecimal as JBigDecimal
// import java.math.BigInteger as JBigInteger
// import java.util.regex.Pattern
// import scala.Boolean as SBoolean
// import scala.Ordering.Implicits.*

// sealed abstract class Primitive[+F[+a] <: Data.Nullable[a], A] extends Codec[F, Data.Primitive, A]:
//   self =>

//   def constraints: Vector[Constraint.Primitive]

//   final override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A] = new Primitive[F, A]:
//     export self.{constraints, decode, default, encode}
//     override def metadata: Metadata = f(self.metadata)

//   final override def modifyDefault(f: Option[A] => Option[A]): Primitive[F, A] = new Primitive[F, A]:
//     export self.{constraints, decode, encode, metadata}
//     override def default: Option[A] = f(self.default)

//   final override def imap[B](f: A => B)(g: B => A): Primitive[F, B] = new Primitive[F, B]:
//     export self.{constraints, metadata}
//     override def default: Option[B] = self.default.map(f)
//     override def decode(data: Data): Codec.Result[B] = self.decode(data).map(f)
//     override def encode(b: B): F[Data.Primitive] = self.encode(g(b))

//   final override def to[B](using convert: Convert[A, B]): Primitive[F, B] = imap(convert.to)(convert.from)

// object Primitive:
//   def number[A <: Double | Int | Float | Long | JBigDecimal | JBigInteger](
//       name: String,
//       minimum: Option[Comparison[A]],
//       maximum: Option[Comparison[A]],
//       multiple: Option[A],
//       gteq: (A, A) => Boolean,
//       gt: (A, A) => Boolean,
//       lteq: (A, A) => Boolean,
//       lt: (A, A) => Boolean,
//       modulo0: (A, A) => Boolean,
//       lift: Data.Number => Option[A],
//       parse: String => Option[A]
//   ): Primitive[Data.Required, A] = new Primitive[Data.Required, A]:
//     override def constraints: Vector[Constraint.Primitive] =
//       minimum.map(_.map(encode)).map(Constraint.Primitive.Minimum.apply).toVector ++
//         maximum.map(_.map(encode)).map(Constraint.Primitive.Maximum.apply).toVector ++
//         multiple.map(encode).map(Constraint.Primitive.Multiple.apply).toVector
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[A] = None

//     def verifyMinimum(value: A): Codec.Result[Unit] = minimum.traverse_ {
//       case comparison @ Comparison(reference, exclusive) =>
//         Validated.cond(
//           if exclusive then gt(value, reference) else gteq(value, reference),
//           (),
//           Violations.rootNec(
//             Violation(Constraint.Primitive.Minimum(comparison.map(Data.Number.apply)), actual = Data.Number(value))
//           )
//         )
//     }

//     def verifyMaximum(value: A): Codec.Result[Unit] = maximum.traverse_ {
//       case comparison @ Comparison(reference, exclusive) =>
//         Validated.cond(
//           if exclusive then lt(value, reference) else lteq(value, reference),
//           (),
//           Violations.rootNec(
//             Violation(Constraint.Primitive.Maximum(comparison.map(Data.Number.apply)), actual = Data.Number(value))
//           )
//         )
//     }

//     def verifyMultiple(value: A): Codec.Result[Unit] = multiple.traverse_ { reference =>
//       Validated.cond(
//         modulo0(value, reference),
//         (),
//         Violations.rootNec(Violation(Constraint.Primitive.Multiple(Data.Number(value)), actual = Data.Number(value)))
//       )
//     }

//     override def decode(data: Data): Codec.Result[A] = data.asPrimitive
//       .flatMap: primitive =>
//         primitive.asNumber.flatMap(lift).orElse(primitive.asString.map(_.value).flatMap(parse))
//       .toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
//       .andThen(value => (verifyMinimum(value) *> verifyMaximum(value) *> verifyMultiple(value)).as(value))

//     override def encode(a: A): Data.Number = Data.Number(a)

//   def jBigDecimal(
//       minimum: Option[Comparison[JBigDecimal]],
//       maximum: Option[Comparison[JBigDecimal]],
//       multiple: Option[JBigDecimal]
//   ): Primitive[Data.Required, JBigDecimal] = number(
//     name = "bigDecimal",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _.remainder(_).compareTo(JBigDecimal.ZERO) == 0,
//     lift = _.toBigDecimal,
//     parse = value =>
//       try Some(new JBigDecimal(value))
//       catch case _: NumberFormatException => None
//   )

//   def jBigInteger(
//       minimum: Option[Comparison[JBigInteger]],
//       maximum: Option[Comparison[JBigInteger]],
//       multiple: Option[JBigInteger]
//   ): Primitive[Data.Required, JBigInteger] = number(
//     name = "bigInteger",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _.mod(_) == 0,
//     lift = _.toBigInteger,
//     parse = value =>
//       try Some(new JBigInteger(value))
//       catch case _: NumberFormatException => None
//   )

//   def double(
//       minimum: Option[Comparison[Double]],
//       maximum: Option[Comparison[Double]],
//       multiple: Option[Double]
//   ): Primitive[Data.Required, Double] = number(
//     name = "double",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _ % _ == 0,
//     lift = _.toDouble,
//     parse = _.toDoubleOption
//   )

//   def float(
//       minimum: Option[Comparison[Float]],
//       maximum: Option[Comparison[Float]],
//       multiple: Option[Float]
//   ): Primitive[Data.Required, Float] = number(
//     name = "float",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _ % _ == 0,
//     lift = _.toFloat,
//     parse = _.toFloatOption
//   )

//   def int(
//       minimum: Option[Comparison[Int]],
//       maximum: Option[Comparison[Int]],
//       multiple: Option[Int]
//   ): Primitive[Data.Required, Int] = number(
//     name = "int",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _ % _ == 0,
//     lift = _.toInt,
//     parse = _.toIntOption
//   )

//   def long(
//       minimum: Option[Comparison[Long]],
//       maximum: Option[Comparison[Long]],
//       multiple: Option[Long]
//   ): Primitive[Data.Required, Long] = number(
//     name = "long",
//     minimum,
//     maximum,
//     multiple,
//     _ >= _,
//     _ > _,
//     _ <= _,
//     _ < _,
//     _ % _ == 0,
//     lift = _.toLong,
//     parse = _.toLongOption
//   )

//   def string(
//       minLength: Option[Int],
//       maxLength: Option[Int],
//       matches: Option[Pattern]
//   ): Primitive[Data.Required, JString] = new Primitive[Data.Required, JString]:
//     override def constraints: Vector[Constraint.Primitive] =
//       minLength.map(Constraint.Primitive.MinLength.apply).toVector ++
//         maxLength.map(Constraint.Primitive.MaxLength.apply).toVector ++
//         matches.map(Constraint.Primitive.Matches.apply).toVector
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[JString] = None

//     def verifyMinLength(value: JString): Codec.Result[Unit] = minLength.traverse_ { reference =>
//       val length = value.length
//       Validated.cond(
//         length >= reference,
//         (),
//         Violations.rootNec(Violation(Constraint.Primitive.MinLength(reference), actual = Data.Number(length)))
//       )
//     }

//     def verifyMaxLength(value: JString): Codec.Result[Unit] = maxLength.traverse_ { reference =>
//       val length = value.length
//       Validated.cond(
//         length <= reference,
//         (),
//         Violations.rootNec(Violation(Constraint.Primitive.MaxLength(reference), actual = Data.Number(length)))
//       )
//     }

//     def verifyMatches(value: JString): Codec.Result[Unit] = matches.traverse_ { pattern =>
//       Validated.cond(
//         pattern.matcher(value).matches(),
//         (),
//         Violations.rootNec(Violation(Constraint.Primitive.Matches(pattern), actual = Data.String(value)))
//       )
//     }

//     override def decode(data: Data): Codec.Result[JString] = data match
//       case Data.String(value) =>
//         (verifyMinLength(value) *> verifyMaxLength(value) *> verifyMatches(value)).as(value)
//       case _ => Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
//     override def encode(a: JString): Data.Primitive = Data.String(a)

//   def parser[A](
//       name: JString,
//       minLength: Option[Int],
//       maxLength: Option[Int],
//       matches: Option[Pattern],
//       f: JString => Option[A],
//       g: A => JString
//   ): Primitive[Data.Required, A] = new Primitive[Data.Required, A]:
//     val codec = string(minLength, maxLength, matches)
//     override def constraints: Vector[Constraint.Primitive] = codec.constraints
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[A] = None
//     override def decode(data: Data): Codec.Result[A] = codec
//       .decode(data)
//       .andThen: value =>
//         f(value).toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(value))))
//     override def encode(a: A): Data.Primitive = codec.encode(g(a))

//   val boolean: Primitive[Data.Required, SBoolean] = new Primitive[Data.Required, SBoolean]:
//     override def constraints: Vector[Constraint.Primitive] = Vector.empty
//     override def metadata: Metadata = Metadata.Empty
//     override def default: Option[SBoolean] = None
//     override def decode(data: Data): Codec.Result[SBoolean] = data.asPrimitive
//       .flatMap: primitive =>
//         primitive.asBoolean.map(_.value).orElse(primitive.asString.flatMap(_.value.toBooleanOption))
//       .toValid(Violations.rootNec(Violation(Constraint.Type("boolean"), actual = Data.String(data.name))))
//     override def encode(a: SBoolean): Data.Primitive = Data.Boolean(a)

//   given [F[+a] <: Data.Nullable[a]]: CodecInvariant[Primitive[F, *]] with
//     override def imap[A, B](fa: Primitive[F, A])(f: A => B)(g: B => A): Primitive[F, B] = fa.imap(f)(g)

//   given [F[+a] <: Data.Nullable[a], A]: Metadata.Ops[Primitive[F, A]] with
//     extension (self: Primitive[F, A])
//       override def metadata: Metadata = self.metadata
//       override def modifyMetadata(f: Metadata => Metadata): Primitive[F, A] = self.modifyMetadata(f)
