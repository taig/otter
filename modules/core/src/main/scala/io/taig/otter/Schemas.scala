package io.taig.otter

import io.taig.otter as Plain
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

trait Schemas extends Syntax:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = Plain.Primitive.Required.Root(Metadata.primitive, tpe)
  final val bigDecimal: Primitive.Required[JBigDecimal] = primitive(Type.BigDecimal)
  final val bigInteger: Primitive.Required[JBigInteger] = primitive(Type.BigInteger)
  final val boolean: Primitive.Required[Boolean] = primitive(Type.Boolean)
  final val double: Primitive.Required[Double] = primitive(Type.Double)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val float: Primitive.Required[Float] = primitive(Type.Float)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
  // final val nonEmptyString: Primitive.Required[Option[String]] = string.imap(_.some.filter(_.nonEmpty))(_.orEmpty)
  // final val password: Primitive.Required[String] = string.format("password")
  // final val uuid: Primitive.Required[UUID] = string.ivalidate(validations.uuid)(_.toString).format("uuid")
  // final val date: Primitive.Required[LocalDate] = string.ivalidate(validations.date)(_.toString).format("date")
  // final val dateTime: Primitive.Required[LocalDateTime] =
  //   string.ivalidate(validations.dateTime)(_.toString).format("date-time")
  // final val cistring: Primitive.Required[CIString] = string.imap(CIString.apply)(_.toString).format("case-insensitive")
  // final val nonEmptyCIString: Primitive.Required[Option[CIString]] = cistring.imap(_.some.filter(_.nonEmpty))(_.orEmpty)
