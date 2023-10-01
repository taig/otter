package io.taig.otter.sample

import cats.effect.{IO, Resource, SyncIO}
import io.taig.otter.http.AppClient
import io.taig.otter.munit.OtterSuite

abstract class SampleSuite extends OtterSuite:
  val app: SyncIO[FunFixture[SampleContext]] =
    val context = SampleApp
      .create(logger = _ => IO.unit)
      .map(app => new SampleClient(AppClient(app)))
      .map(client => SampleContext(client, SampleApi(client)))

    ResourceFixture(Resource.eval(context))
