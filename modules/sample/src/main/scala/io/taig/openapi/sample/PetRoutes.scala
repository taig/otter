package io.taig.openapi.sample

import cats.Applicative
import cats.effect.{Concurrent, IO, Ref}
import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.openapi.dsl.*
import io.taig.openapi.http.{Endpoint, Routes}
import io.taig.openapi.sample.endpoints.pets.Post

final class PetRoutes(authentication: Authentication, storage: Ref[IO, List[Pet]]) extends SampleRoutes(authentication):
  val delete: AuthorizedEndpoint.Implementation[Unit, Unit] = endpoints.pets.delete.infallible(_ => storage.set(Nil))

  val get: UnauthorizedEndpoint.Implementation[Option[Animal], Pets] =
    endpoints.pets.get.infallible { animalFilter =>
      storage.get.map { pets =>
        Pets.unsafeFromList(pets.filter(pet => animalFilter.forall(_ === pet.animal)))
      }
    }

  val post: AuthorizedEndpoint.Implementation[Pet, Either[Post, Pets]] = endpoints.pets.post.raise { pet =>
    storage.get.flatMap { pets =>
      if pets.length >= PetRoutes.MaxPets then IO.raiseError(Post.MaxPetsExceeded(PetRoutes.MaxPets))
      else
        val update = pet :: pets
        storage.set(update).as(Pets.unsafeFromList(update))
    }
  }

  val routes: Routes[IO] = Routes(delete, get, post)

object PetRoutes:
  val MaxPets = 5

  def empty(authentication: Authentication): IO[Routes[IO]] =
    Ref[IO].of(List.empty[Pet]).map(new PetRoutes(authentication, _).routes)
