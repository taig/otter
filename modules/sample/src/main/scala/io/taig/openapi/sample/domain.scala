package io.taig.openapi.sample

import cats.Eq
import cats.syntax.all.*
import io.taig.screening.Validation
import io.taig.screening.validations.*

final case class Pet(name: Pet.Name, animal: Animal)

object Pet:
  opaque type Name = String

  object Name:
    extension (name: Name) def toString: String = name

    def unsafeFromString(value: String): Name = value

    val validation: Validation[Int, String, String, Pet.Name] = text.required <* text.atMost(120)

opaque type Pets = List[Pet]

object Pets:
  extension (pets: Pets) def toList: List[Pet] = pets

  val Empty: Pets = Nil
  val Maximum: Int = 5

  def one(pet: Pet): Pets = List(pet)
  def unsafeFromList(pets: List[Pet]): Pets = pets

  val validation: Validation[Long, List[Pet], List[Pet], Pets] = collection.list.atMost(Maximum).tap

enum Animal:
  case Bird
  case Cat
  case Dog

object Animal:
  given Eq[Animal] = Eq.fromUniversalEquals
