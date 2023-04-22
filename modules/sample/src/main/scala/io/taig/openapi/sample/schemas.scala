package io.taig.openapi.sample

import io.taig.openapi.OpenApi
import io.taig.openapi.dsl.*
import io.taig.openapi.schema.*

object schemas:
  val animal: Enumeration[Animal] = enumeration(string) {
    case Animal.Bird => "bird"
    case Animal.Cat  => "cat"
    case Animal.Dog  => "dog"
  }

  val pet: Product[Pet] =
    val name: Primitive[Pet.Name] = string.ivalidate(Pet.Name.validation)(_.toString)
    (field("name", name) :* field("animal", animal)).gimap

  val pets: Collection[Pets] = collection.list(pet).ivalidate(Pets.validation)(_.toList)
