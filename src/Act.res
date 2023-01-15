type t<'a> = {
  @as("s")
  mutable state: 'a,
  @as("e")
  effects: array<unknown>,
}

let make = initial => {
  state: initial,
  effects: [],
}

let get = act => act.state

let set = (act, state) => act.state = state
