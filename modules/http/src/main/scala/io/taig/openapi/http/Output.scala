package io.taig.openapi.http

import cats.Eval
import cats.data.{Chain, NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.schema.{andThenValidate, Evidence, Schema, Violations}
import io.taig.screening.Validation
import io.taig.screening.identifiers
import io.taig.screening.syntax.*
import org.typelevel.ci.CIString

final case class Output[A](results: Output.Results[A], violations: Output.Result[Violations]):
  def modifyResults[B](f: Output.Results[A] => Output.Results[B]): Output[B] = copy(results = f(results))
  def modifyViolations(f: Output.Result[Violations] => Output.Result[Violations]): Output[A] =
    copy(violations = f(violations))

  def imap[B](f: A => B)(g: B => A): Output[B] = modifyResults(_.imap(f)(g))
  def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Output[B] = modifyResults(
    _.ivalidate(validation)(g)
  )

  def decode(response: Response): Validated[Violations, A] = results.decode(response) match
    case Validated.Valid(Some(a)) => a.valid
    case Validated.Valid(None)    =>
      // if(response.code === failure.code) failure.decode(response)
      ???
    case Validated.Invalid(violations) => violations.invalid

  def encode(a: Validated[Violations, A]): Response = a match
    case Validated.Valid(a)            => results.encode(a)
    case Validated.Invalid(violations) => this.violations.encode(violations)

object Output:
  sealed abstract class Results[A](val toChain: NonEmptyChain[Output.Result[?]]):
    self =>

    final def imap[B](f: A => B)(g: B => A): Output.Results[B] = ivalidate(Validation.fromFunction(f))(g)
    final def gimap[B](using evidence: Evidence.Sum.Aux[B, A]): Output.Results[B] = imap(evidence.from)(evidence.to)
    final def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Output.Results[B] =
      new Results[B](self.toChain):
        override def decode(response: Response): Validated[Violations, Option[B]] =
          self
            .decode(response)
            .andThen(_.traverse(andThenValidate(validation, a => schemas.response.encode(self.encode(a)))))

        override def encode(b: B): Response = self.encode(g(b))

    final infix def orElse[B](results: Output.Results[B]): Output.Results[Either[A, B]] =
      new Results[Either[A, B]](self.toChain ++ results.toChain):
        override def decode(response: Response): Validated[Violations, Option[Either[A, B]]] =
          self
            .decode(response)
            .andThen {
              case Some(a) => a.asLeft.some.valid
              case None    => results.decode(response).map(_.map(_.asRight))
            }
            .findValid(results.decode(response).map(_.map(_.asRight)))
        override def encode(ab: Either[A, B]): Response = ab.fold(self.encode, results.encode)

    final def :+[B](result: Output.Result[B]): Output.Results[Either[A, B]] = orElse(result.toResults)

    def decode(response: Response): Validated[Violations, Option[A]]

    def encode(a: A): Response

  object Results:
    def one[A](result: Result[A]): Output.Results[A] = new Results[A](NonEmptyChain.one(result)):
      override def decode(response: Response): Validated[Violations, Option[A]] =
        if result.code === response.code then result.decode(response).map(_.some) else none[A].valid
      override def encode(a: A): Response = result.encode(a)

  sealed abstract class Result[A](val code: Code, val headers: Headers[?]):
    self =>

    final def imap[B](f: A => B)(g: B => A): Output.Result[B] = ivalidate(Validation.fromFunction(f))(g)
    final def gimap[B](using evidence: Evidence.Sum.Aux[B, A]): Output.Result[B] = imap(evidence.from)(evidence.to)
    final def ivalidate[B](validation: Validation[OpenApi, A, A, B])(g: B => A): Output.Result[B] =
      new Result[B](code, headers):
        override def decodeWithRemainders(response: Response): Validated[Violations, (Response, B)] =
          self
            .decodeWithRemainders(response)
            .andThen(_.traverse(andThenValidate(validation, a => schemas.response.encode(self.encode(a)))))
        override def encode(b: B): Response = self.encode(g(b))

    final def :+[B](result: Output.Result[B]): Output.Results[Either[A, B]] = toResults :+ result

    final def zip[B](values: Headers[B]): Output.Result[(A, B)] = new Result[(A, B)](code, headers.zip(values)):
      override def decodeWithRemainders(response: Response): Validated[Violations, (Response, (A, B))] =
        self.decodeWithRemainders(response).andThen { case (response, a) =>
          values
            .decodeWithRemainders(response.headers)
            .map { case (headers, b) => (response.withHeaders(headers), (a, b)) }
            .leftMap(_.modifyHistory("header" /: _))
        }

      override def encode(ab: (A, B)): Response = self.encode(ab._1).modifyHeaders(_ ++ values.encode(ab._2))

    final def :*[B](header: Header[B]): Output.Result[(A, B)] = zip(header.toHeaders)

    final def decode(response: Response): Validated[Violations, A] = decodeWithRemainders(response).map(_._2)
    def decodeWithRemainders(response: Response): Validated[Violations, (Response, A)]
    def encode(a: A): Response

    final def toResults: Output.Results[A] = Results.one(this)

  object Result:
    def apply(code: Code): Result[Unit] = new Result[Unit](code, Headers.Empty):
      override def decodeWithRemainders(response: Response): Validated[Violations, (Response, Unit)] = Validated.cond(
        response.code === this.code,
        (response, ()),
        Violations.oneNec(
          History.Root / "code",
          identifiers.expected
            .toConstraint(reference = OpenApi.fromInt(this.code.toInt).some)
            .toViolation(actual = OpenApi.fromInt(response.code.toInt))
        )
      )

      override def encode(a: Unit): Response = Response(this.code, headers = Chain.empty, body = none)

    def apply[A](code: Code, body: Eval[Schema[A]]): Result[A] =
      val result: Result[Unit] = Result(code)

      new Result[A](code, Headers.Empty):
        override def decodeWithRemainders(response: Response): Validated[Violations, (Response, A)] =
          result.decode(response) *>
            body.value.decode(response.body.getOrElse(OpenApi.Null)).map(a => (response.withoutBody, a))

        override def encode(a: A): Response = result.encode(()).withBody(body.value.encode(a))
