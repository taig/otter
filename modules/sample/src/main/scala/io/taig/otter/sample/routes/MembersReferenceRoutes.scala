package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.{^, endpoints, Role, Route}
import io.taig.otter.sample.api.endpoints.members.referenceOrSelf.Get
import io.taig.otter.sample.data.{Member, ReferenceOrSelf}
import io.taig.otter.sample.repository.MemberRepository
import io.taig.otter.sample.repository.MemberRepository.Error
import mouse.all.*

final class MembersReferenceRoutes(route: SampleRoute, member: MemberRepository):
  val get: Route[Role.Librarian ^ Role.Member, ReferenceOrSelf[Member.Reference], Either[Get, Member.Summary]] =
    ???
//    route(endpoints.members.referenceOrSelf.get): (self, reference) =>
//      member
//        .findByReference(reference, self)
//        .leftMapIn:
//          case Error.FindByReference.MemberReferenceUnknown => Get.MemberReferenceUnknown
//          case Error.FindByReference.PermissionDenied       => Get.MemberReferenceUnknown

object MembersReferenceRoutes:
  def apply(route: SampleRoute, member: MemberRepository): Routes[IO] =
    val routes = new MembersReferenceRoutes(route, member)
    Routes(routes.get)
