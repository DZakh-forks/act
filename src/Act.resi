type t<'a>
type notify = (. unit) => unit
type unsubscribe = unit => unit

let make: 'a => t<'a>

let get: t<'a> => 'a

let set: (t<'a>, 'a) => unit

let subscribe: (t<'a>, 'a => unit) => unsubscribe

let getNotify: unit => notify

let setNotify: notify => unit
