type t<'a>
type notify = (. unit) => unit
type unsubscribe = unit => unit

let make: 'a => t<'a>

let computed: (~equalityCheck: ('a, 'a) => bool=?, unit => 'a) => t<'a>

@send
external get: t<'a> => 'a = "g"

@send
external set: (t<'a>, 'a) => unit = "s"

let subscribe: (t<'a>, 'a => unit) => unsubscribe

let notify: notify

let wrapNotify: (notify => unit) => unit
