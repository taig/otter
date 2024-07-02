package io.taig.otter

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object TypeStringEncoder:
  def apply[A](tpe: Type[A], a: A): String = tpe match
    case Type.BigDecimal => (a: JBigDecimal).toPlainString
    case Type.BigInteger => (a: JBigInteger).toString
    case Type.Boolean    => String.valueOf(a)
    case Type.Double     => String.valueOf(a)
    case Type.Float      => String.valueOf(a)
    case Type.Int        => String.valueOf(a)
    case Type.Long       => String.valueOf(a)
    case Type.String     => a
