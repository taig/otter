package io.taig.crock.sample

import io.taig.crock.schema.Primitive
import io.taig.crock.schema.schemas.*

object schemas:
  val name: Primitive[User.Name] =
    string
      .ivalidate(User.Name.validation)(_.toString)
      .example(User.Name.unsafeFromString("Bonnie Bonus"))
      .description("Full name, including first, middle and last name")

  val age: Primitive[User.Age] = int.ivalidate(User.Age.validation)(_.toInt)
