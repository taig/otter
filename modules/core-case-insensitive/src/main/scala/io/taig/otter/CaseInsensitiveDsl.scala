package io.taig.otter

import cats.syntax.all.*
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveDsl[Self[_]: Codec]:
  this: PrimitiveDsl.String[Self] =>

  def cistring(
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      matches: Option[Pattern] = none
  ): Self[CIString] = string(minimum, maximum, matches).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring()

  implicit final class ToCIStringCodecOperations(self: cistring.type) extends StringCodecOperations[Self, CIString]:
    override protected def empty: CIString = CIString.empty
    override protected def isEmpty(a: CIString): Boolean = a.isEmpty

    override def apply(minimum: Option[Int], maximum: Option[Int], matches: Option[Pattern]): Self[CIString] =
      cistring(minimum, maximum, matches)
