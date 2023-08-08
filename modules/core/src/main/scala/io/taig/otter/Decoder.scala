package io.taig.otter

trait Decoder[A]:
  self =>
  def decode(openapi: OpenApi): Option[A]

  final def map[B](f: A => B): Decoder[B] = new Decoder[B]:
    override def decode(openapi: OpenApi): Option[B] = self.decode(openapi).map(f)

object Decoder:
  inline def apply[A](using decoder: Decoder[A]): Decoder[A] = decoder

  given Decoder[String] with
    override def decode(openapi: OpenApi): Option[String] = openapi match
      case OpenApi.Text(value) => Some(value)
      case _                   => None

  given Decoder[Boolean] with
    override def decode(openapi: OpenApi): Option[Boolean] = openapi match
      case OpenApi.Bool(value) => Some(value)
      case _                   => None

  given Decoder[BigDecimal] with
    override def decode(openapi: OpenApi): Option[BigDecimal] = openapi match
      case OpenApi.Integer(value: Int)        => Some(BigDecimal(value))
      case OpenApi.Integer(value: Long)       => Some(BigDecimal(value))
      case OpenApi.Integer(value: BigInt)     => Some(BigDecimal(value))
      case OpenApi.Decimal(value: Float)      => Some(BigDecimal(value))
      case OpenApi.Decimal(value: Double)     => Some(BigDecimal(value))
      case OpenApi.Decimal(value: BigDecimal) => Some(value)
      case OpenApi.Text(value) =>
        try Some(BigDecimal(value))
        catch case _: NumberFormatException => None
      case _ => None

  given Decoder[BigInt] with
    override def decode(openapi: OpenApi): Option[BigInt] = openapi match
      case OpenApi.Integer(value: Int)        => Some(BigInt(value))
      case OpenApi.Integer(value: Long)       => Some(BigInt(value))
      case OpenApi.Integer(value: BigInt)     => Some(value)
      case OpenApi.Decimal(value: Float)      => Option.when(value.isWhole)(BigInt(value.toInt))
      case OpenApi.Decimal(value: Double)     => Option.when(value.isWhole)(BigInt(value.toLong))
      case OpenApi.Decimal(value: BigDecimal) => value.toBigIntExact
      case OpenApi.Text(value) =>
        try Some(BigInt(value))
        catch case _: NumberFormatException => None
      case _ => None

  given Decoder[Float] with
    override def decode(openapi: OpenApi): Option[Float] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => Some(value.toFloat)
      case OpenApi.Decimal(value: Double) =>
        Option.when(value >= Float.MinValue && value <= Float.MaxValue)(value.toFloat)
      case OpenApi.Decimal(value: Float)  => Some(value)
      case OpenApi.Integer(value: BigInt) => Option.when(value.isValidFloat)(value.toFloat)
      case OpenApi.Integer(value: Int)    => Some(value.toFloat)
      case OpenApi.Integer(value: Long) =>
        Option.when(value >= Float.MinValue && value <= Float.MaxValue)(value.toFloat)
      case OpenApi.Text(value) => value.toFloatOption
      case _                   => None

  given Decoder[Double] with
    override def decode(openapi: OpenApi): Option[Double] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => Some(value.toDouble)
      case OpenApi.Decimal(value: Double)     => Some(value)
      case OpenApi.Decimal(value: Float)      => Some(value.toDouble)
      case OpenApi.Integer(value: BigInt)     => Some(value.toDouble)
      case OpenApi.Integer(value: Int)        => Some(value.toDouble)
      case OpenApi.Integer(value: Long)       => Some(value.toDouble)
      case OpenApi.Text(value)                => value.toDoubleOption
      case _                                  => None

  given Decoder[Int] with
    override def decode(openapi: OpenApi): Option[Int] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => Option.when(value.isValidInt)(value.toInt)
      case OpenApi.Decimal(value: Double)     => Option.when(value.isValidInt)(value.toInt)
      case OpenApi.Decimal(value: Float)      => Option.when(value.isValidInt)(value.toInt)
      case OpenApi.Integer(value: BigInt)     => Option.when(value.isValidInt)(value.toInt)
      case OpenApi.Integer(value: Int)        => Some(value)
      case OpenApi.Integer(value: Long)       => Option.when(value.isValidInt)(value.toInt)
      case OpenApi.Text(value)                => value.toIntOption
      case _                                  => None

  given Decoder[Long] with
    override def decode(openapi: OpenApi): Option[Long] = openapi match
      case OpenApi.Decimal(value: BigDecimal) => Option.when(value.isValidLong)(value.toLong)
      case OpenApi.Decimal(value: Double) =>
        Option.when(value.isWhole && value >= Long.MinValue && value <= Long.MaxValue)(value.toLong)
      case OpenApi.Decimal(value: Float)  => Option.when(value.isWhole)(value.toLong)
      case OpenApi.Integer(value: BigInt) => Option.when(value.isValidLong)(value.toLong)
      case OpenApi.Integer(value: Int)    => Some(value.toLong)
      case OpenApi.Integer(value: Long)   => Some(value)
      case OpenApi.Text(value)            => value.toLongOption
      case _                              => None
