package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Argument
import org.typelevel.ci.CIString

import java.util.regex.Pattern

trait CaseInsensitiveComponent[+Self[_]: Invariant]:
  this: PrimitiveComponent.String[Self, ?] =>

  def cistring(
      minimum: Argument[Int] = Argument.Default,
      maximum: Argument[Int] = Argument.Default,
      matches: Argument[Pattern] = Argument.Default
  ): Self[CIString] = string(minimum, maximum, matches).imap(CIString.apply)(_.toString)

  val cistring: Self[CIString] = cistring()

  implicit final class ToCIStringCodecOperations(self: cistring.type) extends StringComponentExtension[Self, CIString]:
    override protected def empty: CIString = CIString.empty
    override protected def isEmpty(a: CIString): Boolean = a.isEmpty

    override def apply(minimum: Argument[Int], maximum: Argument[Int], matches: Argument[Pattern]): Self[CIString] =
      cistring(minimum, maximum, matches)
