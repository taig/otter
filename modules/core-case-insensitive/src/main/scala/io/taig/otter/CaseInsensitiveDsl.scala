package io.taig.otter
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveDsl[Self[_]: Codec]:
  this: PrimitiveDsl.String[Self] =>

  def cistring(
      minimum: Argument[Int] = Argument.Default,
      maximum: Argument[Int] = Argument.Default,
      matches: Argument[Pattern] = Argument.Default
  ): Self[CIString] = string(minimum, maximum, matches).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring()

  implicit final class ToCIStringCodecOperations(self: cistring.type) extends StringCodecOperations[Self, CIString]:
    override protected def empty: CIString = CIString.empty
    override protected def isEmpty(a: CIString): Boolean = a.isEmpty

    override def apply(minimum: Argument[Int], maximum: Argument[Int], matches: Argument[Pattern]): Self[CIString] =
      cistring(minimum, maximum, matches)
