package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.Undefined
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveComponent[+Self[_]: Invariant]:
  this: PrimitiveComponent.String[Self] =>

  def cistring(
      minimum: Undefined.Or[Int] = Undefined,
      maximum: Undefined.Or[Int] = Undefined,
      matches: Undefined.Or[Pattern] = Undefined
  ): Self[CIString] = string(minimum, maximum, matches).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring()

  implicit final class ToCIStringCodecOperations(self: cistring.type) extends StringComponentExtension[Self, CIString]:
    override protected def empty: CIString = CIString.empty
    override protected def isEmpty(a: CIString): Boolean = a.isEmpty

    override def apply(
        minimum: Undefined.Or[Int],
        maximum: Undefined.Or[Int],
        matches: Undefined.Or[Pattern]
    ): Self[CIString] =
      cistring(minimum, maximum, matches)
