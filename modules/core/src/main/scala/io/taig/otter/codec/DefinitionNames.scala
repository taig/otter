package io.taig.otter.codec

import scala.annotation.tailrec

/** Assigns output names by reference identity, without traversing recursive schema values. */
opaque type DefinitionNames = Vector[(AnyRef, String)]

object DefinitionNames:
  val Empty: DefinitionNames = Vector.empty

  extension (self: DefinitionNames)
    def get(owner: AnyRef): Option[String] = self.collectFirst:
      case (value, name) if value eq owner => name

    def names: Vector[String] = self.map(_._2)

    def assign(owner: AnyRef, hint: String): (DefinitionNames, String) = get(owner) match
      case Some(name) => (self, name)
      case None       =>
        val name = DefinitionNames.available(hint, names.toSet)
        (self :+ (owner -> name), name)

  def available(hint: String, taken: Set[String]): String =
    @tailrec
    def loop(index: Int): String =
      val name = if index == 1 then hint else s"${hint}_$index"
      if taken.contains(name) then loop(index + 1) else name

    loop(1)
