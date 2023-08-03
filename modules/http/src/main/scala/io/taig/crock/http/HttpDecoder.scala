package io.taig.crock.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.crock.schema.{Decoder, History, StringDecoder, Violations}
import io.taig.crock.validation.Constraint.Equals
import io.taig.crock.validation.{Constraint, Violation}

object HttpDecoder:
  val url: Decoder[Url, (Chain[String], Chain[(String, String)])] = new Decoder:
    override def decode[a](url: Url[a], a: (Chain[String], Chain[(String, String)])): Validated[Violations, a] =
      ???

    def decodeWithRemainders[B](
        url: Url[B],
        path: Chain[String],
        queries: Chain[(String, String)]
    ): Validated[Violations, B] = ???

  val path: Decoder.WithRemainders[Path, Chain[String]] = new Decoder.WithRemainders:
    override def decode[A](path: Path[A], a: Chain[String]): Validated[Violations, A] =
      decodeWithRemainders(path, a).andThen { case (a, remainders) =>
        if remainders.isEmpty then a.valid
        else
          val violation = Violation(Constraint.Equals("/"), actual = remainders.mkString_("/", "/", "").some)
          Violations.rootNec(violation).invalid
      }
    override def decodeWithRemainders[A](
        path: Path[A],
        remainders: Chain[String]
    ): Validated[Violations, (A, Chain[String])] = path match
      case Path.Empty => ((), remainders).valid
      case Path.One(Segment.Static(name)) =>
        remainders.uncons match
          case Some((head, remainders)) =>
            if head === name then ((), remainders).valid
            else Violations.oneNec(History.Root / name, Violation.required(head)).invalid
          case None => Violations.oneNec(History.Root / name, Violation.required).invalid
      case Path.One(Segment.Parameter(name, schema)) =>
        remainders.uncons match
          case Some((head, tail)) =>
            val result = StringDecoder.value.decode(schema.value, head.some).tupleRight(tail)
            if schema.value.isOptional
            then result.orElse(StringDecoder.value.decode(schema.value, None).tupleRight(remainders))
            else result
          case None => Violations.oneNec(History.Root / name, Violation.required).invalid
      case Path.Zip(left, right) =>
        decodeWithRemainders(left, remainders).andThen { case (a, remainders) =>
          decodeWithRemainders(right, remainders).map { case (b, remainders) => ((a, b), remainders) }
        }
      case Path.Modify(self, f, g) => decodeWithRemainders(self, remainders).map(_.leftMap(f))
