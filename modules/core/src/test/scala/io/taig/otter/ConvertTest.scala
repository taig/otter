package io.taig.otter

import cats.syntax.all.*

final class ConvertTest extends OtterSuite:
  enum Animal:
    case Bird
    case Cat(lives: Int)
    case Dog(goodGirl: Boolean)

  test("product"):
    assertEquals(
      obtained = Convert[Int, Animal.Cat].to(7),
      expected = Animal.Cat(lives = 7)
    )

    assertEquals(
      obtained = Convert[Int, Animal.Cat].from(Animal.Cat(lives = 7)),
      expected = 7
    )

  test("sum"):
    assertEquals(
      obtained = Convert[Either[Either[Animal.Bird.type, Animal.Cat], Animal.Dog], Animal]
        .to(Right(Animal.Dog(goodGirl = true))),
      expected = Animal.Dog(goodGirl = true)
    )

    assertEquals(
      obtained = Convert[Either[Either[Animal.Bird.type, Animal.Cat], Animal.Dog], Animal]
        .from(Animal.Cat(lives = 7)),
      expected = Animal.Cat(lives = 7).asRight.asLeft
    )
