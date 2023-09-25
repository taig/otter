package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.{Member, Route}
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.repository.MemberRepository
import io.taig.otter.sample.repository.MemberRepository.Error
import io.taig.otter.sample.api.endpoints.members.Post
import mouse.all.*

final class MembersRoutes(route: SampleRoute, member: MemberRepository):
  val post: Route[Member.Create, Either[Post, Member.Summary]] = route(endpoints.members.post): (_, create) =>
    member
      .create(create)
      .leftMapIn:
        case Error.Create.EmailConflict => Post.EmailConflict

object MembersRoutes:
  def apply(route: SampleRoute, member: MemberRepository): Routes[IO] =
    val routes = new MembersRoutes(route, member)
    Routes(routes.post)
