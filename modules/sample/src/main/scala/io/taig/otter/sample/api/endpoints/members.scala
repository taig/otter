package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.Codec
import io.taig.otter.dsl.*
import io.taig.otter.http.{Endpoint as OtterEndpoint, Result, Results, Url}
import io.taig.otter.sample.api.{^, codecs, parameters, Role}
import io.taig.otter.sample.data.{Member, ReferenceOrSelf, Session}

object members:
  val url: Url[Unit] = __ / "members"

  val get: AuthenticatedEndpoint[Role.Librarian, Unit, Chain[Member.Summary]] = OtterEndpoint(
    request(method.get, url),
    response(result(code.ok, output.json(collection.chain(codecs.member.summary))))
  ).tags("members").role(Role.Librarian)

  enum Post:
    case EmailConflict

  object Post:
    val results: Results[Post] =
      val emailConflict: Codec[EmailConflict.type] = error("emailConflict", dynamic.singleton(EmailConflict))

      result(code.badRequest, output.json(emailConflict)).toResults.to

  val post: AuthenticatedEndpoint[Role.Librarian, Member.Create, Either[Post, Member.Summary]] = OtterEndpoint(
    request(method.post, url, input.json(codecs.member.create)),
    response(Post.results :+ result(code.created, output.json(codecs.member.summary)))
  ).tags("members").role(Role.Librarian)

  object referenceOrSelf:
    val url: Url[ReferenceOrSelf[Member.Reference]] = members.url / parameters.member.referenceOrSelf

    enum Get:
      case MemberReferenceUnknown

    object Get:
      val results: Results[Get] =
        val memberReferenceUnknown: Codec[MemberReferenceUnknown.type] =
          error("memberReferenceUnknown", dynamic.singleton(MemberReferenceUnknown))
        result(code.notFound, output.json(memberReferenceUnknown)).toResults.to

    val get: AuthenticatedEndpoint[Role.Librarian ^ Role.Member, ReferenceOrSelf[Member.Reference], Either[
      Get,
      Member.Summary
    ]] =
      OtterEndpoint(
        request(method.get, url),
        response(Get.results :+ result(code.ok, output.json(codecs.member.summary)))
      ).tags("members").role(Role.Librarian ^ Role.Member)

  object self:
    val url: Url[Unit] = members.url / "self"

    object sessions:
      val url: Url[Unit] = self.url / "sessions"

      enum Post:
        case EmailOrPasswordIncorrect

      object Post:
        val results: Results[Post] =
          val emailOrPasswordIncorrect: Result[EmailOrPasswordIncorrect.type] = result(
            code.unauthorized,
            output.json(error("emailOrPasswordIncorrect", dynamic.singleton(EmailOrPasswordIncorrect)))
          ).description("Email or password incorrect")

          emailOrPasswordIncorrect.toResults.to

      val post: AuthenticatedEndpoint[Role.Guest, Member.Login, Either[Post, Session]] =
        val created = result(code.created, output.json(codecs.session))
          .description("Session successfully created")

        OtterEndpoint(
          request(method.post, url, input.json(codecs.member.login)),
          response(Post.results :+ created)
        ).tags("members").role(Role.Guest)
