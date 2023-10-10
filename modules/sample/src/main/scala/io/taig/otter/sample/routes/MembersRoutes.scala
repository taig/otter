package io.taig.otter.sample.routes

import cats.data.Chain
import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.endpoints.members.Post
import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
import io.taig.otter.sample.data.Member
import io.taig.otter.sample.repository.MemberRepository
import io.taig.otter.sample.repository.MemberRepository.Error
import io.taig.otter.sample.service.EndpointImplementation
import mouse.all.*

final class MembersRoutes(implementation: EndpointImplementation, member: MemberRepository):
  val get: AuthenticatedRoute[Unit, Chain[Member.Summary]] =
    implementation(endpoints.members.get)((_, _) => member.list)

  val post: AuthenticatedRoute[Member.Create, Either[Post, Member.Summary]] = implementation(endpoints.members.post):
    (_, create) =>
      member
        .create(create)
        .leftMapIn:
          case Error.Create.EmailConflict => Post.EmailConflict

object MembersRoutes:
  def apply(implementation: EndpointImplementation, member: MemberRepository): Routes[IO] =
    val routes = new MembersRoutes(implementation, member)
    Routes(routes.get, routes.post)
