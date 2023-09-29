package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.Schema
import io.taig.otter.http.{Results, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.{^, parameters, schemas, Role}
import io.taig.otter.sample.data.Member
import io.taig.otter.sample.data.ReferenceOrSelf

object members:
  val url: Url[Unit] = __ / "members"

  val get: Endpoint[Role.Librarian, Unit, Chain[Member.Summary]] = Endpoint(
    Role.librarian,
    request(method.get, url),
    response(result(code.ok, output.json(collection.chain(schemas.member.summary))))
  )

  enum Post:
    case EmailConflict

  object Post:
    val results: Results[Post] =
      val emailConflict: Schema[EmailConflict.type] = error("emailConflict", dynamic.singleton(EmailConflict))

      result(code.badRequest, output.json(emailConflict)).toResults.to

  val post: Endpoint[Role.Librarian, Member.Create, Either[Post, Member.Summary]] = Endpoint(
    Role.librarian,
    request(method.post, url, input.json(schemas.member.create)),
    response(Post.results :+ result(code.created, output.json(schemas.member.summary)))
  )

  object referenceOrSelf:
    val url: Url[ReferenceOrSelf[Member.Reference]] = members.url / parameters.member.referenceOrSelf

    val get: Endpoint[Role.Librarian ^ Role.Member, ReferenceOrSelf[Member.Reference], Member.Summary] = Endpoint(
      Role.librarian ^ Role.member,
      request(method.get, url),
      response(result(code.ok, output.json(schemas.member.summary)))
    )
