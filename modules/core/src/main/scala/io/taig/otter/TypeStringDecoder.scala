package io.taig.otter

import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object TypeStringDecoder:
  def apply[A](tpe: Type[A], value: String): Option[A] = tpe match
    case Type.BigDecimal =>
      try new JBigDecimal(value).some
      catch { case _: NumberFormatException => none }
    case Type.BigInteger =>
      try new JBigInteger(value).some
      catch { case _: NumberFormatException => none }
    case Type.Boolean => value.toBooleanOption
    case Type.Double  => value.toDoubleOption
    case Type.Float   => value.toFloatOption
    case Type.Int     => value.toIntOption
    case Type.Long    => value.toLongOption
    case Type.String  => value.some
