package io.taig.otter.sample

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all.*
import io.taig.otter.JsonSchemaProfile
import io.taig.otter.codec.JsonSchemaRenderer
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.codec.OpenApiPayload
import io.taig.otter.http.codec.OpenApiRenderer
import io.taig.otter.http.codec.TypescriptEffectPayload
import io.taig.otter.http.codec.TypescriptEndpointRenderer
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.schema

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/** The same endpoint values, written out as documents.
  *
  * Nothing here describes the API a second time. `api.all` is the identical list [[LibraryRoutes]] serves from, read by
  * three different interpreters -- which is the claim the whole library rests on, and the reason a document cannot
  * drift from the server that answers it.
  *
  * Two sides, and they genuinely differ. A server *reads* the request and *writes* the response, and a caller does the
  * reverse; wherever a field is optional or holds a default the two documents disagree, because a reader accepts an
  * absent `metadata` and a writer always produces one. Publishing only the server's document would tell callers a field
  * is required when it is not.
  *
  * Every renderer here returns a document *and* a list of issues. None of them throws, and none half emits: the
  * multipart, streamed and CSV endpoints of [[api.unserved]] are reported by name and the rest of the document still
  * comes back. That is why the issues are printed rather than silently dropped.
  */
object Documents extends IOApp:
  private val Target: Path = Paths.get("target", "sample-library")

  private def write(path: Path, content: String): IO[Unit] =
    IO.blocking(Files.createDirectories(path.getParent)) *>
      IO.blocking(Files.write(path, content.getBytes(StandardCharsets.UTF_8))).void *>
      IO.println(s"wrote $path")

  private def report(label: String, issues: List[Any]): IO[Unit] =
    if issues.isEmpty then IO.println(s"$label: nothing unsaid")
    else IO.println(s"$label: ${issues.length} issue(s)") *> issues.traverse_(issue => IO.println(s"  - $issue"))

  override def run(arguments: List[String]): IO[ExitCode] =
    val payload = OpenApiPayload.json(OpenApiProfile.V31)
    val server = OpenApiRenderer.server(OpenApiProfile.V31, payload).render(api.Info, api.all)
    val client = OpenApiRenderer.client(OpenApiProfile.V31, payload).render(api.Info, api.all)
    val typescript = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json).render(api.all)
    val book = JsonSchemaRenderer.writer(JsonSchemaProfile.Draft202012).render(schema.book)

    write(Target.resolve("openapi.json"), server.value.spaces2) *>
      write(Target.resolve("openapi-client.json"), client.value.spaces2) *>
      write(Target.resolve("api.ts"), typescript.render) *>
      write(Target.resolve("book.schema.json"), book.value.spaces2) *>
      report("openapi", server.issues) *>
      report("typescript", typescript.issues) *>
      report("book.schema.json", book.issues).as(ExitCode.Success)
