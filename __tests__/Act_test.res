module Promise = Js.Promise2
module Array = Js.Array2
module Global = Js.Global
open Ava

test("[ReScript] read outside effect stale computed", t => {
  let a = Act.make(0)
  let b = () => a->Act.get

  t->Assert.is(b(), 0, ())

  a->Act.set(1)
  t->Assert.is(b(), 1, ())
})

test("[ReScript] https://perf.js.hyoo.ru/#!bench=9h2as6_u0mfnn", t => {
  let res = []

  // let numbers = [0, 1]

  // let rec fib = n => n < 2 ? 1 : fib(n - 1) + fib(n - 2)
  // let hard = n => n + fib(16)

  let a = Act.make(0)
  let b = Act.make(0)
  let c = Act.computed(() => a->Act.get->mod(2) + b->Act.get->mod(2))

  let _ = c->Act.subscribe(v => res->Array.push(v)->ignore)

  t->Assert.deepEqual(res, [0], ())
})

test("throw should not broke linking", t => {
  try {
    // @ts-expect-error
    let _ = Act.computed(() => Js.Exn.raiseError("Foo"))->Act.subscribe(_ => ())
  } catch {
  | _ => ()
  }

  let a = Act.make(0)
  let b = Act.computed(() => a->Act.get)
  let c = Act.computed(() => a->Act.get)
  let _ = c->Act.subscribe(_ => ())

  a->Act.set(1)
  t->Assert.deepEqual([a->Act.get, b->Act.get, c->Act.get], [1, 1, 1], ())
})

asyncTest("[ReScript] redefine act.notify", async t => {
  let notify = Act.getNotify()
  Act.setNotify((. ()) => {
    Global.setTimeout(
      () => {
        notify(.)
      },
      0,
    )->ignore
  })

  let a = Act.make(0)
  let callsRef = ref(0)
  let _ = a->Act.subscribe(_ => callsRef.contents = callsRef.contents + 1)

  t->Assert.is(callsRef.contents, 1, ())

  a->Act.set(123)
  await Promise.resolve()
  t->Assert.is(callsRef.contents, 1, ())
  await Promise.make((~resolve, ~reject as _) => {
    Global.setTimeout(() => resolve(. ()), 0)->ignore
  })
  t->Assert.is(callsRef.contents, 2, ())
})
