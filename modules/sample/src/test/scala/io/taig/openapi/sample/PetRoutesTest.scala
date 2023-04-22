package io.taig.openapi.sample

import cats.data.Chain
import cats.effect.std.UUIDGen
import cats.effect.{IO, Resource, SyncIO}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.dsl.*
import io.taig.openapi.http.Request
import munit.CatsEffectSuite
import org.typelevel.ci.*

final class PetRoutesTest extends CatsEffectSuite:
  val client: SyncIO[FunFixture[Clients]] = ResourceFixture {
    Resource.eval(PetRoutes.empty(Authentication.default).map(Clients.default))
  }

  client.test("DELETE /pets") { client =>
    val pet = Pet(Pet.Name.unsafeFromString("Good boy"), Animal.Dog)

    for
      _ <- client.authorized.submitOrFail(endpoints.pets.post, Authentication.Member, pet)
      obtained1 <- client.unauthorized.submitOrError(endpoints.pets.get, none)
      _ <- client.authorized.submitOrError(endpoints.pets.delete, Authentication.Admin, ())
      obtained2 <- client.unauthorized.submitOrError(endpoints.pets.get, none)
    yield
      assertEquals(obtained1, Pets.one(pet))
      assertEquals(obtained2, Pets.Empty)
  }

  client.test("DELETE /pets: unauthorized") { client =>
    for
      token <- UUIDGen.randomUUID[IO]
      obtained <- client.authorized.submitOrAuthorization(endpoints.pets.delete, token, ())
    yield assertEquals(obtained, expected = Authorization.Error.Unauthorized)
  }

  client.test("DELETE /pets: forbidden") { client =>
    for obtained <- client.authorized.submitOrAuthorization(endpoints.pets.delete, Authentication.Member, ())
    yield assertEquals(obtained, expected = Authorization.Error.Forbidden)
  }

  client.test("DELETE /pets: invalid authorization method") { client =>
    val request = Request(
      method.delete,
      path = Chain(OpenApi.fromString("pets")),
      queries = Chain.empty,
      headers = Chain(ci"Authorization" -> OpenApi.fromString("Basic foobar")),
      Request.Body.Singlepart.Empty
    )

    for obtained <- client.underlying.submitRaw(endpoints.pets.delete.endpoint, request)
    yield assertEquals(obtained.code, expected = code.unprocessableEntity)
  }

  client.test("GET /pets") { client =>
    client.unauthorized.submitOrError(endpoints.pets.get, none).map { obtained =>
      assertEquals(obtained, expected = Pets.Empty)
    }
  }

  client.test("POST /pets: filter (type)") { client =>
    val pet = Pet(Pet.Name.unsafeFromString("Good boy"), Animal.Dog)

    for
      _ <- client.authorized.submitOrFail(endpoints.pets.post, Authentication.Member, pet)
      obtained1 <- client.unauthorized.submitOrError(endpoints.pets.get, Animal.Dog.some)
      obtained2 <- client.unauthorized.submitOrError(endpoints.pets.get, Animal.Cat.some)
    yield {
      assertEquals(obtained1, expected = Pets.one(pet))
      assertEquals(obtained2, expected = Pets.Empty)
    }
  }

  client.test("POST /pets") { client =>
    val pet = Pet(Pet.Name.unsafeFromString("Good boy"), Animal.Dog)

    for
      _ <- client.authorized.submitOrFail(endpoints.pets.post, Authentication.Member, pet)
      obtained <- client.unauthorized.submitOrError(endpoints.pets.get, none)
    yield {
      assertEquals(obtained, expected = Pets.one(pet))
    }
  }

  client.test("POST /pets: admin") { client =>
    val pet = Pet(Pet.Name.unsafeFromString("Good boy"), Animal.Dog)

    for
      _ <- client.authorized.submitOrError(endpoints.pets.post, Authentication.Admin, pet)
      obtained <- client.unauthorized.submitOrError(endpoints.pets.get, none)
    yield {
      assertEquals(obtained, expected = Pets.one(pet))
    }
  }

  client.test("POST /pets: unauthorized") { client =>
    val pet = Pet(Pet.Name.unsafeFromString("Good boy"), Animal.Dog)

    for
      token <- UUIDGen.randomUUID[IO]
      obtained <- client.authorized.submitOrAuthorization(endpoints.pets.post, token, pet)
    yield {
      assertEquals(obtained, expected = Authorization.Error.Unauthorized)
    }
  }
