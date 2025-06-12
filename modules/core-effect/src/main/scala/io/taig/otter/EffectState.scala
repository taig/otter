package io.taig.otter

type EffectState[A] = ContextState[Effect[A], Effect[A]]
