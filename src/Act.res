module Array = Js.Array2
module Promise = {
  type t<+'a> = promise<'a>
  @send
  external thenResolve: (t<'a>, 'a => 'b) => t<'b> = "then"
  @val @scope("Promise")
  external resolve: 'a => t<'a> = "resolve"
}
module Fn = {
  let callWithFinally: (. (. unit) => 'a, (. unit) => unit) => 'a = %raw(`(fn, finallyCb) => {
    try {
      return fn()
    } finally {
      finallyCb()
    }
  }`)

  @inline
  let call1 = (fn: 'arg1 => 'return, arg1: 'arg1): 'return => {
    Obj.magic(fn)(. arg1)
  }
}

type notify = (. unit) => unit
type effect = (. unit) => unit
type unsubscribe = unit => unit
type queue = array<array<effect>>

type t<'a> = {
  @as("s")
  mutable state: 'a,
  @as("e")
  mutable valueEffects: array<effect>,
  @as("v")
  mutable _version: float,
}

type rec context = {
  // a subscriber - source of truth
  @as("r")
  mutable maybeRoot: option<effect>,
  // a subscriber to unsubscribe
  @as("u")
  mutable maybeUnroot: option<effect>,
  // effects queue for a batch, also used as a cache key of a transaction
  @as("q")
  mutable queue: queue,
  // global `dirty` flag used to cache visited nodes during it invalidation by a subscriber
  @as("v")
  mutable version: float,
  @as("n")
  mutable notify: internalNotify,
}
and internalNotify = (. context) => unit

type subscribtionContext = {
  @as("q")
  mutable lastQueue: queue,
  @as("s")
  mutable lastState: unknown,
}

let castUnknownToAny: unknown => 'a = Obj.magic
let castAnyToUnknown: 'a => unknown = Obj.magic
let castNotifyToInternal: notify => internalNotify = Obj.magic
let castNotifyFromInternal: internalNotify => notify = Obj.magic

let initialNotify = (. context) => {
  let iterator = context.queue

  context.queue = []

  for effectsIdx in 0 to iterator->Array.length - 1 {
    let effects = iterator->Array.unsafe_get(effectsIdx)
    for effectIdx in 0 to effects->Array.length - 1 {
      let effect = effects->Array.unsafe_get(effectIdx)
      effect(.)
    }
  }
}

let context = {
  maybeRoot: None,
  maybeUnroot: None,
  queue: [],
  version: 0.,
  notify: initialNotify,
}

let getNotify = () =>
  context.notify === initialNotify
    ? (. ()) => initialNotify(. context)
    : context.notify->castNotifyFromInternal

let setNotify = notify => {
  context.notify = notify->castNotifyToInternal
}

let make = initial => {
  state: initial,
  _version: -1.,
  valueEffects: [],
}

@inline
let isComputed = act => act.valueEffects === %raw("undefined")

@inline
let syncEffects = act => {
  if act._version !== context.version {
    act._version = context.version
    switch (context.maybeUnroot, context.maybeRoot) {
    | (Some(unroot), _) =>
      act.valueEffects
      ->Array.removeCountInPlace(~pos=act.valueEffects->Array.indexOf(unroot), ~count=1)
      ->ignore
    | (_, Some(root)) => act.valueEffects->Array.push(root)->ignore
    | _ => ()
    }
  }
}

let get = act => {
  if act->isComputed->not {
    act->syncEffects
  }
  act.state
}

let set = (act, state) => {
  if act->isComputed->not {
    act.state = state

    if context.queue->Array.push(act.valueEffects) === 1 {
      Promise.resolve()->Promise.thenResolve(() => getNotify()(.))->ignore
    }

    act.valueEffects = []

    act->syncEffects
  }
}

let subscribe = (act, cb) => {
  let subscribtionContext = {
    lastQueue: %raw("cb"),
    lastState: %raw("cb"),
  }
  let rec effect = (. ()) => {
    if subscribtionContext.lastQueue !== context.queue {
      subscribtionContext.lastQueue = context.queue

      context.version = context.version +. 1.

      let prevRoot = context.maybeRoot
      context.maybeRoot = Some(effect)

      Fn.callWithFinally(.
        (. ()) => {
          let calculatedState = act->get->castAnyToUnknown
          if subscribtionContext.lastState !== calculatedState {
            subscribtionContext.lastState = calculatedState
            cb->Fn.call1(calculatedState->castUnknownToAny)
          }
        },
        (. ()) => {
          context.maybeRoot = prevRoot
        },
      )
    }
  }

  effect(.)

  () => {
    context.version = context.version +. 1.
    context.maybeUnroot = Some(effect)
    act->get->ignore
    context.maybeUnroot = None
  }
}
