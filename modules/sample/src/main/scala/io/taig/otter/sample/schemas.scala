package io.taig.otter.sample

import cats.data.Chain
import io.circe.Json
import io.taig.otter.circe.schemas.dynamic.json
import io.taig.otter.sample.User.{Age, Name}
import io.taig.otter.schema.*
import io.taig.otter.schema.schemas.*
import io.taig.otter.validation.validations.*

object schemas:
  val name: Primitive[User.Name] = string
    .ivalidate(User.Name.validation)(_.toString)
    .example(User.Name.unsafeFromString("Bonnie Bonus"))
    .description("Full name, including first, middle and last name")

  val age: Primitive[User.Age] = int.ivalidate(User.Age.validation)(_.toInt)

  val names: Collection[Primitive, Chain[User.Name]] =
    collection.chain(name).validate(minItems(3)).validate(maxItems(10))

  val gender: Enumeration[User.Gender] = enumeration(string):
    case User.Gender.Male             => "male"
    case User.Gender.Female           => "female"
    case User.Gender.ApacheHelicopter => "apacheHelicopter"

  val user: Record[User] = (
    field("name", name) :*
      field("age", age) :*
      field("gender", gender.optional) :*
      field("props", json)
  ).to

  val userProduct: Product[User] = (name :* age :* gender.optional :* json).to
