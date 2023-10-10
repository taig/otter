package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
import io.taig.otter.sample.api.endpoints.members.self.sessions.Post
import io.taig.otter.sample.data.{Member, Session}
import io.taig.otter.sample.repository.MemberRepository
import io.taig.otter.sample.service.EndpointImplementation

final class MembersSelfSessionsRoutes(implementation: EndpointImplementation, members: MemberRepository):
  val post: AuthenticatedRoute[Member.Login, Either[Post, Session]] =
    implementation(endpoints.members.self.sessions.post): (_, login) =>
      IO(???)

object MembersSelfSessionsRoutes:
  def apply(implementation: EndpointImplementation, member: MemberRepository): Routes[IO] =
    val routes = new MembersSelfSessionsRoutes(implementation, member)
    Routes(routes.post)
