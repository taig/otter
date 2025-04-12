package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

final class HttpHeaderParser(explode: Boolean) extends Parser[Http.Header]:
  override def apply[A](codec: Http.Header[A], value: String): Validated[Violations, A] = codec match
    case codec: Http.Header.Array[A]  => apply(codec, value)
    case codec: Http.Header.Object[A] => apply(codec, value)
    case codec: Http.Header.Value[A]  => apply(codec, value)

  def apply[A](codec: Http.Header.Array[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Array.Collection(self) => apply(codec = self, value)
    case Http.Header.Array.Tuple(self)      => apply(codec = self, value)

  def apply[A](codec: Collection[Http.Header.Value, A], value: String): Validated[Violations, A] =
    apply(codec, values = HttpHeaderParser.parser.array(value))

  def apply[A](codec: Collection[Http.Header.Value, A], values: List[String]): Validated[Violations, A] =
    codec match
      case Collection.Indexed(codec, _, _, _, _) =>
        values.toVector.zipWithIndex
          .traverse((value, index) => apply(codec = codec.value, unescape(value, ",")).leftMap(index /: _))
      case Collection.Linked(codec, _, _, _, _) =>
        values.zipWithIndex
          .traverse((value, index) => apply(codec = codec.value, unescape(value, ",")).leftMap(index /: _))
      case Collection.Modify(self, f, _) => apply(codec = self, values).map(f)

  def apply[A](codec: Tuple[Http.Header.Value, A], value: String): Validated[Violations, A] =
    val values = HttpHeaderParser.parser.array(value)
    val reference = codec.codecs.size.toInt
    val actual = values.size

    Validated.cond(
      test = actual <= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference), actual, hint = none))
    ) *> Validated.cond(
      test = actual >= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.MinItems(reference), actual, hint = none))
    ) *> apply(codec, values, index = 0)

  def apply[A](codec: Tuple[Http.Header.Value, A], values: List[String], index: Int): Validated[Violations, A] =
    codec match
      case Tuple.Empty(_)           => ().valid
      case Tuple.Modify(self, f, _) => apply(codec = self, values, index).map(f)
      case Tuple.Root(codec, _) =>
        values.headOption
          .toValid(Violations.rootNec(Violation.required))
          .andThen(apply(codec = codec.value, _))
          .leftMap(index /: _)
      case Tuple.Zip(left, right, _) =>
        val size = left.codecs.size.toInt
        values
          .splitAt(size)
          .bimap(apply(codec = left, _, index), apply(codec = right, _, index + size))
          .tupled

  def apply[A](codec: Http.Header.Object[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Object.Dictionary(self) => apply(codec = self, value)
    case Http.Header.Object.Record(self)     => apply(codec = self, value)

  def apply[A](codec: Dictionary[Http.Header.Value, Http.Header.Value, A], value: String): Validated[Violations, A] =
    obj(value).andThen(apply(codec, _))

  def apply[A](
      codec: Dictionary[Http.Header.Value, Http.Header.Value, A],
      values: List[(String, String)]
  ): Validated[Violations, A] = codec match
    case Dictionary.Root(key, codec, _, _, _) =>
      values.traverse: (name, value) =>
        (apply(codec = key.value, name), apply(codec = codec.value, value)).tupled.leftMap(name /: _)
    case Dictionary.Modify(self, f, _) => apply(codec = self, values).map(f)

  def apply[A](codec: Record[Http.Header.Value, Http.Header.Value, A], value: String): Validated[Violations, A] =
    obj(value).andThen(apply(codec, _).map((_, a) => a))

  def apply[A](
      codec: Record[Http.Header.Value, Http.Header.Value, A],
      values: List[(String, String)]
  ): Validated[Violations, (List[(String, String)], A)] = codec match
    case Record.Empty(_) => (values, ()).valid
    case Record.Field(key, value, _) =>
      val name = HttpHeaderPrinter(explode = false)(codec = key.self.value, key.value)
      val (remainders, result) = collectFirstWithRemainders(values) { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen(apply(codec = value.self.value, _))
        .leftMap(name /: _)
        .tupleLeft(remainders)
    case Record.Modify(self, f, _) => apply(codec = self, values).map(_.map(f))
    case Record.Optional(self) =>
      val keys = self.fields.map((key, _) => HttpHeaderPrinter(explode = false)(codec = key.self.value, key.value))
      val references = values.map((key, _) => key).toSet

      if keys.forall(!references.contains(_))
      then (values, none).valid
      else apply(codec = self, values).map(_.map(_.some))
    case Record.Zip(left, right, _) =>
      apply(codec = left, values) match
        case Validated.Valid((values, a)) =>
          apply(codec = right, values) match
            case Validated.Valid((values, b))      => (values, (a, b)).valid
            case violations @ Validated.Invalid(_) => violations
        case Validated.Invalid(left) =>
          apply(codec = right, values) match
            case Validated.Valid((_, _))           => left.invalid
            case violations @ Validated.Invalid(_) => violations

  def apply[A](codec: Http.Header.Value[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Value.Constant(self)    => apply(codec, value)
    case Http.Header.Value.Enumeration(self) => apply(codec, value)
    case Http.Header.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
    case Http.Header.Value.Union(self)       => apply(codec, value)

  def apply[A](codec: Constant[Http.Header.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
      case Constant.Root(codec, _) =>
        val reference = PrimitivePrinter.Unquoted(codec = codec.self.value.self, codec.value)
        Validated.cond(
          test = value === reference,
          codec.value,
          Violations.rootNec(Violation.equal(reference, actual = value))
        )

  def apply[A](codec: Enumeration[Http.Header.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Enumeration.Modify(self, f, g) => apply(codec = self, value).map(f)
      case self @ Enumeration.Root(codec, mapping, _) =>
        PrimitiveParser
          .Unquoted(codec = codec.value.self, value)
          .andThen: a =>
            mapping
              .unapply(a)
              .toValid:
                val values = self.values.toList
                  .map(mapping.apply)
                  .map(PrimitivePrinter.Unquoted(codec = codec.value.self, _))
                Violations.rootNec(Violation.oneOf(values, actual = value))

  def apply[A](codec: Union.Untagged[Http.Header.Value, A], value: String): Validated[Violations, A] = codec match
    case Union.Untagged.Branch(_, codec, _) => apply(codec = codec.value, value)
    case Union.Untagged.Modify(self, f, _)  => apply(codec = self, value).map(f)
    case Union.Untagged.OrElse(left, right, _) =>
      apply(codec = left, value).map(Left(_)).findValid(apply(codec = right, value).map(Right(_)))

  def obj(value: String): Validated[Violations, List[(String, String)]] =
    val values =
      if explode
      then HttpHeaderParser.parser.obj.exploded(value)
      else HttpHeaderParser.parser.obj.unexploded(value)

    values.toValidated.leftMap: error =>
      Violations.rootNec(Violation.tpe(name = "object", actual = value, hint = error.show))

object HttpHeaderParser:
  def apply(explode: Boolean = false): Parser[Http.Header] = new HttpHeaderParser(explode)

  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    val escaped = (character: Char) =>
      charWhere(value => value != '\\' && value != character).orElse(char('\\') *> anyChar)

    def array(value: String): List[String] = split(value, ",").toList.map(unescape(_, ","))

    object obj:
      val exploded: String => Either[Error, List[(String, String)]] =
        val key = escaped('=').rep.string
        val value = escaped(',').rep.string
        val parser = (key.with1 ~ (char('=') *> value)).repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, "="), unescape(_, List(",", "=")))))

      val unexploded: String => Either[Error, List[(String, String)]] =
        val value = escaped(',').rep.string
        val parser = (value.with1 ~ (char(',') *> value)).repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, ","), unescape(_, ","))))
