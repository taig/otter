// package io.taig.otter.sample.routes

// import cats.effect.IO
// import io.taig.otter.http.Routes
// import io.taig.otter.sample.api.endpoints.members.referenceOrSelf.Get
// import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
// import io.taig.otter.sample.data.{Member, ReferenceOrSelf}
// import io.taig.otter.sample.repository.MemberRepository
// import io.taig.otter.sample.repository.MemberRepository.Error
// import io.taig.otter.sample.service.EndpointImplementation
// import mouse.all.*

// final class MembersReferenceRoutes(implementation: EndpointImplementation, member: MemberRepository):
//   val get: AuthenticatedRoute[ReferenceOrSelf[Member.Reference], Either[Get, Member.Summary]] =
//     implementation(endpoints.members.referenceOrSelf.get): (self, reference) =>
//       member
//         .findByReference(reference, self)
//         .leftMapIn:
//           case Error.FindByReference.MemberReferenceUnknown => Get.MemberReferenceUnknown
//           case Error.FindByReference.PermissionDenied       => Get.MemberReferenceUnknown

// object MembersReferenceRoutes:
//   def apply(implementation: EndpointImplementation, member: MemberRepository): Routes[IO] =
//     val routes = new MembersReferenceRoutes(implementation, member)
//     Routes(routes.get)
