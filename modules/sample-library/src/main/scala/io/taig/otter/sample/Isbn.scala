package io.taig.otter.sample

import cats.Order
import cats.Show

/** A book's identifier, held as the thirteen digits it is and never as the text it arrived as.
  *
  * Opaque because the representation is the point: a hyphenated `978-0-261-10221-7` and a bare `9780261102217` are one
  * ISBN, and a type that let either through as a `String` would leave every comparison and every map key depending on
  * which spelling a caller happened to send. Normalising on the way in is what makes [[Isbn.parse]] the way to get one
  * from the wire, and the schema that reads it is a `codec` rather than a `parser` because there is a way back:
  * [[Isbn.render]] writes the digits.
  *
  * Bounded by `String` rather than left wholly abstract, and that bound is load bearing rather than a convenience.
  * [[io.taig.otter.Append]] is a match type that asks whether what it is appending is a tuple or a `Unit`, and a match
  * type cannot reduce against a type it knows nothing about: an unbounded `opaque type Isbn = String` is abstract
  * everywhere but this file, so a record holding one would fail to find its `Convert` with a message about fields not
  * being covered in the correct order. Naming the upper bound tells the compiler what it needs and gives nothing away
  * -- reading an `Isbn` as text is what [[Isbn.render]] already offers, and there is still no way to make one except
  * through [[Isbn.parse]].
  */
opaque type Isbn <: String = String

object Isbn:
  extension (self: Isbn)
    /** The thirteen digits, with nothing between them. */
    def value: String = self

  /** Reads an ISBN in any of the spellings people write one in.
    *
    * Hyphens and spaces are separators rather than content, so they are dropped before the length is counted. What is
    * left has to be thirteen digits: this is ISBN-13, and the ten digit form is a different identifier rather than a
    * shorter one.
    */
  def parse(value: String): Either[String, Isbn] =
    val trimmed = value.filterNot(character => character == '-' || character == ' ')

    if trimmed.length == 13 && trimmed.forall(_.isDigit) then Right(trimmed)
    else Left("not a thirteen digit ISBN")

  /** The digits, taken as given.
    *
    * Visible to this package and no further, for the seed catalogue and the tests, where the text is a literal written
    * a few lines away. Reading those back through [[Isbn.parse]] would be asking at runtime what is already settled in
    * the source, and would leave every fixture holding an `Either` it has no way to be rid of.
    */
  private[sample] def digits(value: String): Isbn = value

  given Order[Isbn] = Order.by(_.value)

  given Show[Isbn] = Show.show(_.value)
