type t<'a>
type notify = (. unit) => unit
type unsubscribe = unit => unit

let make: 'a => t<'a>

let computed: (unit => 'a) => t<'a>

@send
external get: t<'a> => 'a = "g"

@send
external set: (t<'a>, 'a) => unit = "s"

let subscribe: (t<'a>, 'a => unit) => unsubscribe

let getNotify: unit => notify

let setNotify: notify => unit
