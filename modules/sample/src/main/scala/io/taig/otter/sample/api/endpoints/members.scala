package io.taig.otter.sample.api.endpoints

import io.taig.otter.Schema
import io.taig.otter.http.{Results, Url}
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.{Member, Role}
import io.taig.otter.sample.api.schemas

object members:
  val url: Url[Unit] = __ / "members"

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
