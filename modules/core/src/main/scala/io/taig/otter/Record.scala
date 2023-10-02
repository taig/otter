package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Record[A](val description: Option[String], val nulls: Null) extends Schema[A]:
  self =>
  final override type Self[a] = Record[a]

  def toChain: Chain[Field[?]]

  final override def description(f: Option[String] => Option[String]): Record[A] = Record(this, f(description), nulls)
  final def nulls(f: Null => Null): Record[A] = Record(this, description, f(nulls))

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

  def toProduct: Product[A]

  final override def optional: Record[Option[A]] = new Record[Option[A]](description, nulls):
    export self.{constraints, toChain}
    override def isOptional: Boolean = true
    override def toProduct: Product[Option[A]] = self.toProduct.optional
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], Option[A])] = data match
      case Some(_) => self.decodeWithRemainders(data).map(_.map(_.some))
      case None    => (data, none).valid
    override def encode(a: Option[A], nulls: Null): Option[Chain[(String, Data)]] = a.flatMap(self.encode(_, nulls))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Record[B] =
    new Record[B](description, nulls):
      export self.{isOptional, toChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def toProduct: Product[B] = self.toProduct.ivalidate(validation)(g)
      override def decodeWithRemainders(
          data: Option[Chain[(String, Data)]]
      ): Validated[Violations, (Option[Chain[(String, Data)]], B)] =
        self.decodeWithRemainders(data).andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encode(b: B, nulls: Null): Option[Chain[(String, Data)]] = self.encode(g(b), nulls)

  final def product[B](schema: Record[B]): Record[(A, B)] = new Record[(A, B)](None, Null.Default):
    override def toChain: Chain[Field[?]] = self.toChain ++ schema.toChain
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[(A, B)] = self.toProduct.product(schema.toProduct)
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], (A, B))] =
      self.decodeWithRemainders(data).andThen { case (data, a) =>
        schema.decodeWithRemainders(data).map(_.tupleLeft(a))
      }
    override def encode(ab: (A, B), nulls: Null): Option[Chain[(String, Data)]] =
      (self.encode(ab._1, nulls), schema.encode(ab._2, nulls)) match
        case (Some(a), Some(b))  => Some(a ++ b)
        case (a @ Some(_), None) => a
        case (None, b @ Some(_)) => b
        case (None, None)        => None

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(Data.Object(values)) => decodeWithRemainders(Some(values)).map(_._2)
    case Some(data)                => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
    case None                      => decodeWithRemainders(None).map(_._2)
  def decodeWithRemainders(
      data: Option[Chain[(String, Data)]]
  ): Validated[Violations, (Option[Chain[(String, Data)]], A)]
  final override def encode(a: A): Data = encode(a, nulls).map(Data.Object.apply).getOrElse(Data.Null)
  protected def encode(a: A, nulls: Null): Option[Chain[(String, Data)]]

object Record extends ToRecordOps:
  def apply[A](schema: Record[A], description: Option[String], nulls: Null): Record[A] =
    new Record[A](description, nulls) { export schema.* }

  val Empty: Record[Unit] = new Record[Unit](None, Null.Default):
    override def toChain: Chain[Field[?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[Unit] = Product.Empty
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], Unit)] = (data, ()).valid
    override def encode(a: Unit, nulls: Null): Option[Chain[(String, Data)]] = Chain.empty.some

  def apply[A](field: Field[A]): Record[A] = new Record[A](None, Null.Default):
    override def toChain: Chain[Field[?]] = Chain.one(field)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[A] = Product(field.schema)
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], A)] = data match
      case Some(data) => field.decodeWithRemainders(data).map(_.leftMap(_.some))
      case None       => Violations.rootNec(Violation.required).invalid
    override def encode(a: A, nulls: Null): Option[Chain[(String, Data)]] = field.encode(a, nulls).some
