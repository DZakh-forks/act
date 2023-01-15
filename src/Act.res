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
module Exn = {
  type error

  @new
  external makeError: string => error = "Error"

  let raiseError = (error: error): 'a => error->Obj.magic->raise
}

type notify = (. unit) => unit
type effect = (. unit) => unit
type unsubscribe = unit => unit
type queue = array<array<effect>>

type t<'a> = {
  @as("a")
  state: 'a,
  @as("g")
  get: unit => 'a,
  @as("s") @dead
  set: 'a => unit,
}
type pub<'a> = {
  @as("a")
  act: t<'a>,
  @as("s")
  stateSnapshot: 'a,
}
type valueAct<'a> = {
  @as("a")
  mutable state: 'a,
  @as("v")
  mutable version: float,
  @as("e")
  mutable effects: array<effect>,
  @as("g") @dead
  mutable get: unit => 'a,
  @as("s") @dead
  mutable set: 'a => unit,
}
type computedAct<'a> = {
  @as("a")
  mutable state: 'a,
  @as("v")
  mutable version: float,
  @as("p")
  mutable pubs: array<pub<unknown>>,
  @as("g") @dead
  mutable get: unit => 'a,
  @as("s") @dead
  mutable set: 'a => unit,
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
  // list of a publishers from a computed in prev stack step
  @as("p")
  mutable maybePubs: option<array<pub<unknown>>>,
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
let castAnyActToUnknownAct: t<'a> => t<unknown> = Obj.magic
let castUnknownActToAnyAct: t<unknown> => t<'a> = Obj.magic
let castNotifyToInternal: notify => internalNotify = Obj.magic
let castNotifyFromInternal: internalNotify => notify = Obj.magic
let castValueActToGeneric: valueAct<'a> => t<'a> = Obj.magic
let castComputedActToGeneric: computedAct<'a> => t<'a> = Obj.magic

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
  maybePubs: None,
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

@inline
let addPublisher = (act: t<'a>) => {
  switch context.maybePubs {
  | Some(pubs) =>
    pubs
    ->Array.push({
      act,
      stateSnapshot: act.state,
    })
    ->ignore

  | None => ()
  }
}

@inline
let syncEffects = (valueAct: valueAct<'a>) => {
  if valueAct.version !== context.version {
    valueAct.version = context.version
    switch (context.maybeUnroot, context.maybeRoot) {
    | (Some(unroot), _) =>
      valueAct.effects
      ->Array.removeCountInPlace(~pos=valueAct.effects->Array.indexOf(unroot), ~count=1)
      ->ignore
    | (_, Some(root)) => valueAct.effects->Array.push(root)->ignore
    | _ => ()
    }
  }
}

let make = initial => {
  let valueAct = {
    state: initial,
    version: -1.,
    effects: [],
    get: %raw("undefined"),
    set: %raw("undefined"),
  }

  {
    let valueAct = valueAct->(Obj.magic: valueAct<'a> => valueAct<unknown>)
    valueAct.get = () => {
      valueAct->syncEffects
      valueAct->castValueActToGeneric->addPublisher
      valueAct.state->castUnknownToAny
    }
    valueAct.set = state => {
      let state = state->castAnyToUnknown
      valueAct.state = state

      if context.queue->Array.push(valueAct.effects) === 1 {
        Promise.resolve()->Promise.thenResolve(() => getNotify()(.))->ignore
      }

      valueAct.effects = []

      valueAct->syncEffects
      valueAct->castValueActToGeneric->addPublisher
    }
  }

  valueAct->castValueActToGeneric
}

let computed = fn => {
  let computedAct = {
    state: %raw("undefined"),
    version: -1.,
    pubs: [],
    get: %raw("undefined"),
    set: _ => {
      Exn.raiseError(Exn.makeError("Act.set is not supported for computed acts."))
    },
  }

  {
    let computedAct = computedAct->(Obj.magic: computedAct<'a> => computedAct<unknown>)
    computedAct.get = () => {
      if computedAct.version !== context.version || context.maybeRoot === None {
        let computedPubs = computedAct.pubs
        let prevPubs = context.maybePubs
        context.maybePubs = None

        let isEmptyComputedPubs = computedPubs->Array.length === 0
        if (
          isEmptyComputedPubs ||
          computedPubs->Array.some(el =>
            (el.act->castUnknownActToAnyAct).get->Fn.call1() !== el.stateSnapshot
          )
        ) {
          let newPubs = isEmptyComputedPubs ? computedPubs : []
          context.maybePubs = Some(newPubs)
          computedAct.pubs = newPubs
          computedAct.state = fn->Fn.call1()->castAnyToUnknown

          // TODO:
          // let newState = computed()
          // if (_version === -1 || !equal?.(s, newState)) s = newState
        }

        context.maybePubs = prevPubs

        computedAct.version = context.version
      }

      computedAct->castComputedActToGeneric->addPublisher
      computedAct.state->castUnknownToAny
    }
  }

  computedAct->castComputedActToGeneric
}

@send
external get: t<'a> => 'a = "g"

@send
external set: (t<'a>, 'a) => unit = "s"

let subscribe = (act, cb) => {
  let act = act->castAnyActToUnknownAct
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
          let calculatedState = act->get
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
