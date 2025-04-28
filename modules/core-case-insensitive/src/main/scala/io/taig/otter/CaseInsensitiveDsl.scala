package io.taig.otter

import cats.syntax.all.*
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveDsl[Self[_]: Codec]:
  this: PrimitiveDsl.String[Self] =>

  def cistring(
      minimum: Value[Int] = Value.Default,
      maximum: Value[Int] = Value.Default,
      matches: Value[Pattern] = Value.Default
  ): Self[CIString] = string(minimum, maximum, matches).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring()

  implicit final class ToCIStringCodecOperations(self: cistring.type) extends StringCodecOperations[Self, CIString]:
    override protected def empty: CIString = CIString.empty
    override protected def isEmpty(a: CIString): Boolean = a.isEmpty

    override def apply(minimum: Value[Int], maximum: Value[Int], matches: Value[Pattern]): Self[CIString] =
      cistring(minimum, maximum, matches)
