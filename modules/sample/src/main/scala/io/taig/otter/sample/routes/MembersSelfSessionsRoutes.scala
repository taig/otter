package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.{endpoints, Route}
import io.taig.otter.sample.api.endpoints.members.self.sessions.Post
import io.taig.otter.sample.data.Member
import io.taig.otter.sample.repository.{LibrarianRepository, MemberRepository}

final class MembersSelfSessionsRoutes(route: SampleRoute, members: MemberRepository):
  val post: Route[Member.Login, Either[Post, Member.Session]] =
    route(endpoints.members.self.sessions.post): (_, login) =>
      IO(???)

object MembersSelfSessionsRoutes:
  def apply(route: SampleRoute, member: MemberRepository): Routes[IO] =
    val routes = new MembersSelfSessionsRoutes(route, member)
    Routes(routes.post)
