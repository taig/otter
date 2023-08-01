package io.taig.crock.sample

import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.sample.User.Name
import io.taig.crock.schema.{Collection, Primitive}
import io.taig.crock.schema.schemas.*
import io.taig.crock.validation.validations.*

object schemas:
  val name: Primitive[User.Name] = string
    .ivalidate(User.Name.validation)(_.toString)
    .example(User.Name.unsafeFromString("Bonnie Bonus"))
    .description("Full name, including first, middle and last name")

  val age: Primitive[User.Age] = int.ivalidate(User.Age.validation)(_.toInt)

  val names: Collection.Of[Primitive, Chain[User.Name]] =
    collection.chain(name).validate(minItems(3)).validate(maxItems(10))
