package io.taig.otter

import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Codec[A]:
  self =>
  type Self[a] <: Codec[a]
  type Optional[a] <: Codec[a]

  def constraints: Chain[Constraint]
  def isOptional: Boolean

  def description: Option[String]
  def description(f: Option[String] => Option[String]): Self[A]
  final def description(value: Option[String]): Self[A] = description(_ => value)
  final def description(value: String): Self[A] = description(Some(value))

  def name: Option[String]
  def name(f: Option[String] => Option[String]): Self[A]
  final def name(value: Option[String]): Self[A] = name(_ => value)
  final def name(value: String): Self[A] = name(Some(value))

  def optional: Optional[Option[A]]

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
  final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
  final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)

  final def :+[B](codec: Codec[B]): Union.Of[this.type | codec.type, Either[A, B]] = toUnion.orElse(codec.toUnion)
  final def +:[B](codec: Codec[B]): Union.Of[this.type | codec.type, Either[B, A]] = codec.toUnion.orElse(toUnion)

  def encode(a: A): Data
  final def decode(data: Data): Validated[Violations, A] = decode(data.asValue)
  def decode(data: Option[Data.Value]): Validated[Violations, A]

  def toUnion: Union.Of[this.type, A] = Union(this)

object Codec:
  extension [A <: Matchable](self: Codec[A])
    inline def |[B <: Matchable](codec: Codec[B]): Union.Of[self.type | codec.type, A | B] = (self :+ codec).imap {
      case Left(a)  => a
      case Right(b) => b
    } {
      case a: A => Left(a)
      case b: B => Right(b)
    }

sealed trait Value[A] extends Codec[A]:
  override type Self[a] <: Value[a]
  override type Optional[a] <: Value[a]

  def print(a: A): String | Option[String]
  def parse(value: Option[String]): Validated[Violations, A]

object Value:
  sealed trait Required[A] extends Value[A]:
    override type Self[a] <: Value.Required[a]

    final def :+[B](codec: Value.Required[B]): Union.Required.Of[this.type | codec.type, Either[A, B]] =
      toUnion.orElse(codec.toUnion)
    final def +:[B](codec: Value.Required[B]): Union.Required.Of[this.type | codec.type, Either[B, A]] =
      codec.toUnion.orElse(toUnion)

    final override def toUnion: Union.Required.Of[this.type, A] = Union.Required(this)

    override def encode(a: A): Data.Primitive
    override def print(a: A): String
    final override def parse(value: Option[String]): Validated[Violations, A] =
      Validated.fromOption(value, Violations.rootNec(Violation.required)).andThen(parse)
    def parse(value: String): Validated[Violations, A]

  object Required:
    extension [A <: Matchable](self: Value.Required[A])
      inline def |[B <: Matchable](codec: Value.Required[B]): Union.Required.Of[self.type | codec.type, A | B] =
        (self :+ codec).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

sealed abstract class Collection[A](val codec: Codec[?], val description: Option[String], val name: Option[String])
    extends Codec[A]:
  self =>
  override type Self[a] = Collection.Of[Of, a]
  override type Optional[a] = Collection.Of[Of, a]
  type Of <: Codec[?]

  final override def description(f: Option[String] => Option[String]): Collection.Of[Of, A] =
    new Collection[A](self.codec, description, name) { export self.* }

  final override def name(f: Option[String] => Option[String]): Collection.Of[Of, A] =
    new Collection[A](self.codec, description, name) { export self.* }

  final override def optional: Collection.Of[Of, Option[A]] = new Collection[Option[A]](codec, description, None):
    export self.{constraints, Of}
    override def isOptional: Boolean = true
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decodeArray(data).map(_.some))
    override def encodeArray(a: Option[A]): Option[Chain[Data]] = a.flatMap(self.encodeArray)
    override def parse(values: Option[Chain[String]])(using Of <:< Value[?]): Validated[Violations, Option[A]] =
      values.fold(none.valid)(_ => self.parse(values).map(_.some))
    override def print(a: Option[A])(using Of <:< Value[?]): Option[Chain[String]] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] =
    new Collection[B](codec, description, None):
      export self.{isOptional, Of}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decodeArray(data: Option[Data.Array]): Validated[Violations, B] =
        self.decodeArray(data).andThen(validation(_).leftMap(Violations.root))
      override def encodeArray(b: B): Option[Chain[Data]] = self.encodeArray(g(b))
      override def parse(values: Option[Chain[String]])(using Of <:< Value[?]): Validated[Violations, B] =
        self.parse(values).andThen(validation(_).leftMap(Violations.root))
      override def print(b: B)(using Of <:< Value[?]): Option[Chain[String]] = self.print(g(b))

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Array) => decodeArray(data.asInstanceOf[Option[Data.Array]])
    case Some(data)          => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
    case None                => decodeArray(None)
  def decodeArray(data: Option[Data.Array]): Validated[Violations, A]
  final override def encode(a: A): Data = encodeArray(a).map(Data.Array.apply).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Chain[Data]]

  def parse(values: Option[Chain[String]])(using Of <:< Value[?]): Validated[Violations, A]
  def print(a: A)(using Of <:< Value[?]): Option[Chain[String]]

object Collection:
  type Of[A <: Codec[?], B] = Collection[B] { type Of <: A }

  def apply[F[a] <: Codec[a], A](of: F[A]): Collection.Of[F[A], Chain[A]] = new Collection[Chain[A]](of, None, None):
    override type Of = F[A]
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Chain[A]] = Validated
      .fromOption(data, Violations.rootNec(Violation.required))
      .andThen(_.values.zipWithIndex.traverse { case (data, index) =>
        of.decode(data).leftMap(_.modifyHistory(index /: _))
      })
    override def encodeArray(a: Chain[A]): Option[Chain[Data]] = a.map(of.encode).some
    override def parse(values: Option[Chain[String]])(using F[A] <:< Value[?]): Validated[Violations, Chain[A]] =
      Validated
        .fromOption(values, Violations.rootNec(Violation.required))
        .andThen(_.zipWithIndex.traverse { case (value, index) =>
          of.asInstanceOf[Value[A]].parse(value.some).leftMap(_.modifyHistory(index /: _))
        })
    override def print(a: Chain[A])(using F[A] <:< Value[?]): Option[Chain[String]] = a.mapFilter { a =>
      of.asInstanceOf[Value[A]].print(a) match
        case value: String         => value.some
        case value: Option[String] => value
    }.some

sealed abstract class Coproduct[A](
    val description: Option[String],
    val discriminator: Discriminator,
    val name: Option[String]
) extends Codec[A]:
  self =>
  final override type Self[a] = Coproduct[a]
  final override type Optional[a] = Coproduct[a]

  def toNonEmptyChain: NonEmptyChain[Branch[?, ?]]

  final override def description(f: Option[String] => Option[String]): Coproduct[A] =
    new Coproduct[A](f(description), discriminator, None) { export self.* }

  final def discriminator(f: Discriminator => Discriminator): Coproduct[A] =
    new Coproduct[A](description, f(discriminator), None) { export self.* }

  final override def name(f: Option[String] => Option[String]): Coproduct[A] =
    new Coproduct[A](description, discriminator, f(name)) { export self.* }

  final def to[B](using evidence: Evidence.Coproduct.Aux[B, A]): Coproduct[B] = imap(evidence.from)(evidence.to)

  final override def optional: Coproduct[Option[A]] =
    new Coproduct[Option[A]](description, Discriminator.Default, None):
      export self.{constraints, toNonEmptyChain}
      override def isOptional: Boolean = true
      override def decode(
          data: Option[Chain[(String, Data)]],
          discriminator: Discriminator
      ): Validated[Violations, Option[Option[A]]] =
        data.fold(none.valid)(_ => self.decode(data, discriminator).map(_.some))
      override def encode(a: Option[A], discriminator: Discriminator): Option[Chain[(String, Data)]] =
        a.flatMap(self.encode(_, discriminator))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Coproduct[B] =
    new Coproduct[B](description, discriminator, None):
      export self.{isOptional, toNonEmptyChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decode(
          data: Option[Chain[(String, Data)]],
          discriminator: Discriminator
      ): Validated[Violations, Option[B]] = self
        .decode(data, discriminator)
        .andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encode(b: B, discriminator: Discriminator): Option[Chain[(String, Data)]] =
        self.encode(g(b), discriminator)

  final def orElse[B](codec: Coproduct[B]): Coproduct[Either[A, B]] =
    new Coproduct[Either[A, B]](None, Discriminator.Default, None):
      override def toNonEmptyChain: NonEmptyChain[Branch[?, ?]] = self.toNonEmptyChain ++ codec.toNonEmptyChain
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decode(
          data: Option[Chain[(String, Data)]],
          discriminator: Discriminator
      ): Validated[Violations, Option[Either[A, B]]] = self
        .decode(data, discriminator)
        .map(_.map(_.asLeft))
        .andThen:
          case a @ Some(_) => a.valid
          case None        => codec.decode(data, discriminator).map(_.map(_.asRight))
      override def encode(ab: Either[A, B], discriminator: Discriminator): Option[Chain[(String, Data)]] =
        ab.fold(self.encode(_, discriminator), codec.encode(_, discriminator))

  final def :+[B](branch: Branch[?, B]): Coproduct[Either[A, B]] = self.orElse(branch.toCoproduct)
  final def +:[B](branch: Branch[?, B]): Coproduct[Either[B, A]] = branch.toCoproduct.orElse(self)

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(data @ Data.Object(values)) =>
      decode(Some(values), discriminator).andThen:
        case Some(a) => a.valid
        case None =>
          val values = toNonEmptyChain.toChain.map(branch => Data.String(branch.print))
          Violations.rootNec(Violation(Constraint.OneOf(values), actual = data)).invalid
    case Some(data) => Violations.rootNec(Violation.tpe("object", actual = data.name)).invalid
    case None =>
      decode(None, discriminator).andThen:
        case Some(a) => a.valid
        case None =>
          val values = toNonEmptyChain.toChain.map(branch => Data.String(branch.print))
          Violations.rootNec(Violation(Constraint.OneOf(values), actual = data.getOrElse(Data.Null))).invalid
  def decode(data: Option[Chain[(String, Data)]], discriminator: Discriminator): Validated[Violations, Option[A]]

  final override def encode(a: A): Data = encode(a, discriminator).map(Data.Object.apply).getOrElse(Data.Null)
  protected def encode(a: A, discriminator: Discriminator): Option[Chain[(String, Data)]]

object Coproduct:
  def apply[A](branch: Branch[?, A]): Coproduct[A] = new Coproduct[A](None, Discriminator.Default, None):
    override def toNonEmptyChain: NonEmptyChain[Branch[?, ?]] = NonEmptyChain.one(branch)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(
        data: Option[Chain[(String, Data)]],
        discriminator: Discriminator
    ): Validated[Violations, Option[A]] =
      Validated.fromOption(data, Violations.rootNec(Violation.required)).andThen(branch.decode(_, discriminator))
    override def encode(a: A, discriminator: Discriminator): Option[Chain[(String, Data)]] =
      branch.encode(a, discriminator).some

sealed abstract class Dictionary[A](val codec: Codec[?], val description: Option[String], val name: Option[String])
    extends Codec[A]:
  self =>
  final override type Self[a] = Dictionary[a]
  final override type Optional[a] = Dictionary[a]

  final override def description(f: Option[String] => Option[String]): Dictionary[A] =
    new Dictionary[A](codec, f(description), None) { export self.* }

  final override def name(f: Option[String] => Option[String]): Dictionary[A] =
    new Dictionary[A](codec, description, f(name)) { export self.* }

  final override def optional: Dictionary[Option[A]] = new Dictionary[Option[A]](codec, description, None):
    export self.constraints
    override def isOptional: Boolean = true
    override def decodeObject(data: Option[Data.Object]): Validated[Violations, Option[A]] =
      self.decodeObject(data).map(_.some)
    override def encodeObject(a: Option[A]): Option[Data.Object] = a.flatMap(self.encodeObject)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    new Dictionary[B](codec, description, None):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decodeObject(data: Option[Data.Object]): Validated[Violations, B] =
        self.decodeObject(data).andThen(validation(_).leftMap(Violations.root))
      override def encodeObject(b: B): Option[Data.Object] = self.encodeObject(g(b))

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Object) => decode(data.asInstanceOf[Option[Data.Object]])
    case Some(data)           => Violations.rootNec(Violation.tpe("object", data.name)).invalid
    case None                 => decode(None)
  def decodeObject(data: Option[Data.Object]): Validated[Violations, A]

  final override def encode(a: A): Data = encodeObject(a).getOrElse(Data.Null)
  def encodeObject(a: A): Option[Data.Object]

object Dictionary:
  def apply[A, B](key: Value.Required[A], of: Codec[B]): Dictionary[Chain[(A, B)]] =
    new Dictionary[Chain[(A, B)]](of, None, None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decodeObject(data: Option[Data.Object]): Validated[Violations, Chain[(A, B)]] = data match
        case Some(data) =>
          data.values.traverse { case (a, b) =>
            (key.parse(a), of.decode(b)).tupled.leftMap(_.modifyHistory(a /: _))
          }
        case None => Violations.rootNec(Violation.required).invalid
      override def encodeObject(a: Chain[(A, B)]): Option[Data.Object] =
        Data.Object(a.map { case (a, b) => (key.print(a), of.encode(b)) }).some

sealed abstract class Dynamic[A](val description: Option[String], val name: Option[String]) extends Codec[A]:
  self =>
  final override type Self[a] = Dynamic[a]
  final override type Optional[a] = Dynamic[a]

  final override def description(f: Option[String] => Option[String]): Dynamic[A] =
    new Dynamic[A](f(description), None) { export self.* }

  override def name(f: Option[String] => Option[String]): Dynamic[A] =
    new Dynamic[A](description, f(name)) { export self.* }

  final override def optional: Dynamic[Option[A]] = new Dynamic[Option[A]](description, None):
    export self.constraints
    override def isOptional: Boolean = true
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decode(data).map(_.some))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dynamic[B] =
    new Dynamic[B](description, None):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def encode(b: B): Data = self.encode(g(b))
      override def decode(data: Option[Data.Value]): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))

object Dynamic:
  val Default: Dynamic[Data.Value] = new Dynamic[Data.Value](None, None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(a: Data.Value): Data = a
    override def decode(data: Option[Data.Value]): Validated[Violations, Data.Value] =
      Validated.fromOption(data, Violations.rootNec(Violation.required))

sealed abstract class Enumeration[A] extends Value[A]:
  self =>
  override type Self[a] <: Enumeration[a]
  final override type Optional[a] = Enumeration[a]

  def codec: Value[?]

  def values: Chain[Data.Primitive]

  final override def optional: Enumeration[Option[A]] =
    new Enumeration.Optional[Option[A]](self.codec, self.description, None):
      export self.values
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
        data.fold(none.valid)(_ => self.decode(data).map(_.some))
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Validated[Violations, Option[A]] =
        value.fold(none.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a
        .map(self.print)
        .flatMap:
          case value: String         => value.some
          case value: Option[String] => value

object Enumeration:
  sealed abstract class Required[A](
      val codec: Value.Required[?],
      val description: Option[String],
      val name: Option[String]
  ) extends Enumeration[A]
      with Value.Required[A]:
    self =>
    final override type Self[a] = Enumeration.Required[a]
    final override def isOptional: Boolean = false
    final override def description(f: Option[String] => Option[String]): Enumeration.Required[A] =
      new Required[A](codec, f(description), None) { export self.* }
    final override def name(f: Option[String] => Option[String]): Enumeration.Required[A] =
      new Required[A](codec, description, f(name)) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration.Required[B] =
      new Required[B](codec, description, None):
        export self.values
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive = self.encode(g(b))
        override def parse(value: String): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))

  object Required:
    def apply[A, B](of: Value.Required[A], mapping: Mapping[B, A]): Enumeration.Required[B] =
      new Required[B](of, None, None):
        override def values: Chain[Data.Primitive] = Chain.fromSeq(mapping.values.map(encode))
        override def constraints: Chain[Constraint] = Chain.empty
        override def decode(data: Option[Data.Value]): Validated[Violations, B] = of
          .decode(data)
          .andThen: a =>
            Validated.fromOption(
              mapping.prj(a),
              Violations.rootNec(Violation(Constraint.OneOf(values), data.getOrElse(Data.Null)))
            )
        override def encode(b: B): Data.Primitive = of.encode(mapping.inj(b))
        override def parse(value: String): Validated[Violations, B] = of
          .parse(value)
          .andThen: a =>
            Validated.fromOption(
              mapping.prj(a),
              Violations.rootNec(Violation(Constraint.OneOf(values), Data.String(value)))
            )
        override def print(b: B): String = of.print(mapping.inj(b))

  abstract private class Optional[A](val codec: Value[?], val description: Option[String], val name: Option[String])
      extends Enumeration[A]:
    self =>
    final override type Self[a] = Enumeration.Optional[a]
    final override def isOptional: Boolean = false
    final override def description(f: Option[String] => Option[String]): Enumeration.Optional[A] =
      new Enumeration.Optional[A](codec, f(description), None) { export self.* }
    final override def name(f: Option[String] => Option[String]): Enumeration.Optional[A] =
      new Enumeration.Optional[A](codec, description, f(name)) { export self.* }
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration.Optional[B] =
      new Enumeration.Optional[B](codec, description, None):
        export self.values
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String | Option[String] = self.print(g(b))

sealed abstract class Primitive[A] extends Codec[A] with Value[A]:
  self =>
  override type Self[a] <: Primitive[a]
  final override type Optional[a] = Primitive[a]

  def tpe: Type[?]

  def format: Option[String]
  def format(f: Option[String] => Option[String]): Self[A]
  final def format(value: Option[String]): Self[A] = format(_ => value)
  final def format(value: String): Self[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] =
    new Primitive.Optional[Option[A]](self.description, self.format, self.tpe, None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
        data.fold(none.valid)(_ => self.decode(data).map(_.some))
      override def encode(a: Option[A]): Data.Primitive | Data.Null.type = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Validated[Violations, Option[A]] =
        value.fold(none.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a
        .map(self.print)
        .flatMap:
          case value: String         => value.some
          case value: Option[String] => value

  override def encode(a: A): Data.Primitive | Data.Null.type

object Primitive:
  sealed abstract class Required[A](
      val description: Option[String],
      val format: Option[String],
      val tpe: Type[?],
      val name: Option[String]
  ) extends Primitive[A]
      with Value.Required[A]:
    self =>
    final override type Self[a] = Primitive.Required[a]

    final override def isOptional: Boolean = false

    final override def description(f: Option[String] => Option[String]): Primitive.Required[A] =
      new Required[A](f(description), format, tpe, None) { export self.* }

    final override def format(f: Option[String] => Option[String]): Primitive.Required[A] =
      new Required[A](description, f(format), tpe, None) { export self.* }

    final override def name(f: Option[String] => Option[String]): Primitive.Required[A] =
      new Required[A](description, format, tpe, f(name)) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive.Required[B] =
      new Required[B](description, format, tpe, None):
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive = self.encode(g(b))
        override def parse(value: String): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))

    override def encode(a: A): Data.Primitive

  object Required:
    def apply[A](of: Type[A]): Primitive.Required[A] = new Required[A](None, None, of, None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
        case Some(data: Data.Primitive) => of.decode(data)
        case Some(data)                 => Violations.rootNec(Violation.tpe(of.name, actual = data.name)).invalid
        case None                       => Violations.rootNec(Violation.required).invalid
      override def encode(a: A): Data.Primitive = of.encode(a)
      override def parse(value: String): Validated[Violations, A] = Validated.fromOption(
        of.parse(value),
        Violations.rootNec(Violation.tpe(of.name, actual = value))
      )
      override def print(a: A): String = of.print(a)

  abstract private class Optional[A](
      val description: Option[String],
      val format: Option[String],
      val tpe: Type[?],
      val name: Option[String]
  ) extends Primitive[A]:
    self =>
    final override type Self[a] = Primitive.Optional[a]

    final override def isOptional: Boolean = true

    final override def description(f: Option[String] => Option[String]): Primitive.Optional[A] =
      new Primitive.Optional[A](f(description), format, tpe, None) { export self.* }

    final override def format(f: Option[String] => Option[String]): Primitive.Optional[A] =
      new Primitive.Optional[A](description, f(format), tpe, None) { export self.* }

    final override def name(f: Option[String] => Option[String]): Primitive.Optional[A] =
      new Primitive.Optional[A](description, format, tpe, f(name)) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive.Optional[B] =
      new Primitive.Optional[B](description, format, tpe, None):
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive | Data.Null.type = self.encode(g(b))
        override def parse(value: Option[String]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String | Option[String] = self.print(g(b))

    override def encode(a: A): Data.Primitive | Data.Null.type

sealed abstract class Product[A](val description: Option[String], val name: Option[String]) extends Codec[A]:
  self =>
  final override type Self[a] = Product[a]
  final override type Optional[a] = Product[a]

  def toChain: Chain[Codec[?]]

  final override def description(f: Option[String] => Option[String]): Product[A] =
    new Product[A](f(description), None) { export self.* }

  final override def name(f: Option[String] => Option[String]): Product[A] =
    new Product[A](description, f(name)) { export self.* }

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Product[B] = imap(evidence.from)(evidence.to)

  final override def optional: Product[Option[A]] = new Product[Option[A]](description, None):
    override def toChain: Chain[Codec[?]] = self.toChain
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, Option[A])] =
      if data.values.forall(_ == Data.Null)
      then (Data.Array.Empty, none).valid
      else self.decodeArrayWithRemainders(data).map(_.map(_.some))
    override def encodeArray(a: Option[A]): Option[Data.Array] = a.flatMap(self.encodeArray)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Product[B] =
    new Product[B](description, None):
      override def toChain: Chain[Codec[?]] = self.toChain
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def isOptional: Boolean = self.isOptional
      override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, B)] =
        self.decodeArrayWithRemainders(data).andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encodeArray(b: B): Option[Data.Array] = self.encodeArray(g(b))

  final def product[B](codec: Product[B]): Product[(A, B)] = new Product[(A, B)](None, None):
    override def toChain: Chain[Codec[?]] = self.toChain ++ codec.toChain
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, (A, B))] =
      self.decodeArrayWithRemainders(data).andThen { case (data, a) =>
        codec.decodeArrayWithRemainders(data).map(_.tupleLeft(a))
      }
    override def encodeArray(ab: (A, B)): Option[Data.Array] =
      (self.encodeArray(ab._1), codec.encodeArray(ab._2)) match
        case (Some(a), Some(b)) => Some(a ++ b)
        case (Some(a), None)    => Some(a ++ Data.Array.fill(codec.toChain.length)(Data.Null))
        case (None, Some(b))    => Some(Data.Array.fill(self.toChain.length)(Data.Null) ++ b)
        case (None, None)       => None

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(data: Data.Array) =>
      val length = toChain.length
      if data.length < length
      then Violations.rootNec(Violation(Constraint.MinItems(length), actual = Data.Number(data.length))).invalid
      else if data.length > length
      then Violations.rootNec(Violation(Constraint.MaxItems(length), actual = Data.Number(data.length))).invalid
      else decodeArrayWithRemainders(data).map(_._2)
    case Some(data) => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
    case None       => decodeArrayWithRemainders(Data.Array.fill(toChain.length)(Data.Null)).map(_._2)
  def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, A)]

  final override def encode(a: A): Data = encodeArray(a).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Data.Array]

object Product:
  val Empty: Product[Unit] = new Product[Unit](None, None):
    override def toChain: Chain[Codec[?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, Unit)] =
      (data, ()).valid
    override def encodeArray(a: Unit): Option[Data.Array] = Data.Array.Empty.some

  def apply[A](codec: Codec[A]): Product[A] = new Product[A](None, None):
    override def toChain: Chain[Codec[?]] = Chain.one(codec)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArrayWithRemainders(data: Data.Array): Validated[Violations, (Data.Array, A)] =
      data.values.uncons match
        case Some(head, tail) => codec.decode(head).tupleLeft(Data.Array(tail))
        case None             => Violations.rootNec(Violation.required).invalid
    override def encodeArray(a: A): Option[Data.Array] = Data.Array(Chain.one(codec.encode(a))).some

sealed abstract class Record[A](val description: Option[String], val name: Option[String], val nulls: Null)
    extends Codec[A]:
  self =>
  final override type Self[a] = Record[a]
  final override type Optional[a] = Record[a]

  def toChain: Chain[Field[?]]

  final override def description(f: Option[String] => Option[String]): Record[A] =
    new Record[A](f(description), None, nulls) { export self.* }

  final override def name(f: Option[String] => Option[String]): Record[A] =
    new Record[A](description, f(name), nulls) { export self.* }

  final def nulls(f: Null => Null): Record[A] = new Record[A](description, None, f(nulls)) { export self.* }

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Record[B] = imap(evidence.from)(evidence.to)

  def toProduct: Product[A]

  final override def optional: Record[Option[A]] = new Record[Option[A]](description, None, nulls):
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
    new Record[B](description, None, nulls):
      export self.{isOptional, toChain}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def toProduct: Product[B] = self.toProduct.ivalidate(validation)(g)
      override def decodeWithRemainders(
          data: Option[Chain[(String, Data)]]
      ): Validated[Violations, (Option[Chain[(String, Data)]], B)] =
        self.decodeWithRemainders(data).andThen(_.traverse(validation(_).leftMap(Violations.root)))
      override def encode(b: B, nulls: Null): Option[Chain[(String, Data)]] = self.encode(g(b), nulls)

  final def product[B](codec: Record[B]): Record[(A, B)] = new Record[(A, B)](None, None, Null.Default):
    override def toChain: Chain[Field[?]] = self.toChain ++ codec.toChain
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[(A, B)] = self.toProduct.product(codec.toProduct)
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], (A, B))] =
      self.decodeWithRemainders(data).andThen { case (data, a) =>
        codec.decodeWithRemainders(data).map(_.tupleLeft(a))
      }
    override def encode(ab: (A, B), nulls: Null): Option[Chain[(String, Data)]] =
      (self.encode(ab._1, nulls), codec.encode(ab._2, nulls)) match
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
  val Empty: Record[Unit] = new Record[Unit](None, None, Null.Default):
    override def toChain: Chain[Field[?]] = Chain.empty
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[Unit] = Product.Empty
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], Unit)] = (data, ()).valid
    override def encode(a: Unit, nulls: Null): Option[Chain[(String, Data)]] = Chain.empty.some

  def apply[A](field: Field[A]): Record[A] = new Record[A](None, None, Null.Default):
    override def toChain: Chain[Field[?]] = Chain.one(field)
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toProduct: Product[A] = Product(field.codec)
    override def decodeWithRemainders(
        data: Option[Chain[(String, Data)]]
    ): Validated[Violations, (Option[Chain[(String, Data)]], A)] = data match
      case Some(data) => field.decodeWithRemainders(data).map(_.leftMap(_.some))
      case None       => Violations.rootNec(Violation.required).invalid
    override def encode(a: A, nulls: Null): Option[Chain[(String, Data)]] = field.encode(a, nulls).some

sealed abstract class Union[A] extends Codec[A]:
  self =>
  override type Self[a] <: Union.Of[Of, a]
  final override type Optional[a] = Union.Of[Of, a]
  type Of <: Codec[?]

  def toNonEmptyChain: NonEmptyChain[Codec[?]]

  final override def optional: Union.Of[Of, Option[A]] = ???

  final def orElse[B](codec: Union[B]): Union.Of[self.Of | codec.Of, Either[A, B]] =
    new Union.Root[Either[A, B]](None, None):
      override type Of = self.Of | codec.Of
      override def toNonEmptyChain: NonEmptyChain[Codec[?]] = self.toNonEmptyChain.concat(codec.toNonEmptyChain)
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] =
        self.decode(data).map(_.asLeft).findValid(codec.decode(data).map(_.asRight))
      override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, codec.encode)
      override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, Either[A, B]] =
        self.parse(value).map(_.asLeft).findValid(codec.parse(value).map(_.asRight))
      override def print(ab: Either[A, B])(using Of <:< Value[?]): Option[String] = ab.fold(self.print, codec.print)

  def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, A]
  def print(a: A)(using Of <:< Value[?]): Option[String]

object Union:
  type Of[A <: Codec[?], B] = Union[B] { type Of <: A }

  sealed abstract class Required[A](val description: Option[String], val name: Option[String]) extends Union[A]:
    self =>
    final override type Self[a] = Union.Required.Of[Of, a]
    override type Of <: Value.Required[?]
    final override def isOptional: Boolean = false

    final override def description(f: Option[String] => Option[String]): Required.Of[Of, A] =
      new Required[A](f(description), None) { export self.* }

    final override def name(f: Option[String] => Option[String]): Required.Of[Of, A] =
      new Required[A](description, f(name)) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Required.Of[Of, B] =
      new Required[B](description, None):
        export self.{toNonEmptyChain, Of}
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))
    final def orElse[B](codec: Union.Required[B]): Union.Required.Of[self.Of | codec.Of, Either[A, B]] =
      new Required[Either[A, B]](None, None):
        export self.Of
        override def toNonEmptyChain: NonEmptyChain[Codec[?]] = self.toNonEmptyChain.concat(codec.toNonEmptyChain)
        override def constraints: Chain[Constraint] = Chain.empty
        override def decode(data: Option[Data.Value]): Validated[Violations, Either[A, B]] =
          self.decode(data).map(_.asLeft).findValid(codec.decode(data).map(_.asRight))
        override def encode(ab: Either[A, B]): Data = ab.fold(self.encode, codec.encode)
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, Either[A, B]] =
          self.parse(value).map(_.asLeft).findValid(codec.parse(value).map(_.asRight))
        override def print(ab: Either[A, B]): String = ab.fold(self.print, codec.print)
    final override def print(a: A)(using Of <:< Value[?]): Option[String] = print(a).some

    final def parse(value: String): Validated[Violations, A] = parse(Some(value))
    def print(a: A): String

  object Required:
    type Of[A <: Value.Required[?], B] = Union.Required[B] { type Of <: A }

    def apply[A](codec: Value.Required[A]): Union.Required.Of[codec.type, A] = new Required[A](None, None):
      override type Of = codec.type
      override def toNonEmptyChain: NonEmptyChain[Codec[?]] = NonEmptyChain.one(codec)
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, A] = codec.decode(data)
      override def encode(a: A): Data = codec.encode(a)
      override def parse(value: Option[String])(using codec.type <:< Value[?]): Validated[Violations, A] =
        codec.parse(value)
      override def print(a: A): String = codec.print(a)

  abstract private class Root[A](val description: Option[String], val name: Option[String]) extends Union[A]:
    self =>
    override type Self[a] = Union.Of[self.Of, a]
    final override def description(f: Option[String] => Option[String]): Union.Of[self.Of, A] =
      new Root[A](f(description), None) { export self.* }
    final override def name(f: Option[String] => Option[String]): Union.Of[self.Of, A] =
      new Root[A](description, f(name)) { export self.* }
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Union.Of[self.Of, B] =
      new Root[B](description, None):
        export self.{isOptional, toNonEmptyChain, Of}
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String])(using Of <:< Value[?]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B)(using Of <:< Value[?]): Option[String] = self.print(g(b))

  def apply[A](codec: Codec[A]): Union.Of[codec.type, A] = new Root[A](None, None):
    override type Of = codec.type
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def toNonEmptyChain: NonEmptyChain[Codec[?]] = NonEmptyChain.one(codec)
    override def decode(data: Option[Data.Value]): Validated[Violations, A] = codec.decode(data)
    override def encode(a: A): Data = codec.encode(a)
    override def parse(value: Option[String])(using codec.type <:< Value[?]): Validated[Violations, A] =
      codec.asInstanceOf[Value[A]].parse(value)
    override def print(a: A)(using codec.type <:< Value[?]): Option[String] =
      codec.asInstanceOf[Value[A]].print(a) match
        case value: String         => value.some
        case value: Option[String] => value
