type t<'a>

let make: 'a => t<'a>

let get: t<'a> => 'a

let set: (t<'a>, 'a) => unit
