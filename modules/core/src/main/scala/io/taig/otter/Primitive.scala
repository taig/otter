package io.taig.otter
import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

abstract class Primitive[A] extends Value[A]:
  override type Self[a] = Primitive[a]

  override def optional: Primitive[Option[A]] = ???

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = ???

  override def orElse[B](schema: Value[B]): Value[Either[A, B]] = ???

  def orElse[B](schema: Primitive[B]): Primitive[Either[A, B]] = ???

object Primitive:
  def apply[A](tpe: Type[A]): Primitive[A] = new Primitive[A]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def description: Option[String] = None
    override def description(f: Option[String] => Option[String]): Primitive[A] = ???
    override def decode(data: Data): Validated[Violations, A] = ???
    override def encode(a: A): Data = ???
    override def parse(value: String): Validated[Violations, A] =
      val result: Option[A] = tpe match
        case Type.BigDecimal =>
          try Some(BigDecimal(value))
          catch case _: NumberFormatException => None
        case Type.BigInt =>
          try Some(BigInt(value))
          catch case _: NumberFormatException => None
        case Type.Boolean => value.toBooleanOption
        case Type.Double  => value.toDoubleOption
        case Type.Float   => value.toFloatOption
        case Type.Int     => value.toIntOption
        case Type.Long    => value.toLongOption
        case Type.String  => Some(value)
      Validated.fromOption(result, Violations.rootNec(Violation.tpe(tpe.name, actual = value)))
    override def print(a: A): String = tpe match
      case Type.BigDecimal => a.toString
      case Type.BigInt     => a.toString
      case Type.Boolean    => String.valueOf(a)
      case Type.Double     => String.valueOf(a)
      case Type.Float      => String.valueOf(a)
      case Type.Int        => String.valueOf(a)
      case Type.Long       => String.valueOf(a)
      case Type.String     => String.valueOf(a)
