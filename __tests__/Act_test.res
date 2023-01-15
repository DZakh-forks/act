module Promise = Js.Promise2
module Array = Js.Array2
module Global = Js.Global
open Ava

@@warning("-21")

test("read outside effect stale computed", t => {
  let a = Act.make(0)
  let b = () => a->Act.get

  t->Assert.is(b(), 0, ())

  a->Act.set(1)
  t->Assert.is(b(), 1, ())
})

test("https://perf.js.hyoo.ru/#!bench=9h2as6_u0mfnn", t => {
  let res = []

  let numbers = [0, 1]

  let rec fib = n => n < 2 ? 1 : fib(n - 1) + fib(n - 2)
  let hard = (n, _string: string) => n + fib(16)

  let a = Act.make(0)
  let b = Act.make(0)
  let c = Act.computed(() => a->Act.get->mod(2) + b->Act.get->mod(2))
  let d = Act.computed(
    ~equalityCheck=(l, r) => {
      l->Array.length === r->Array.length && l->Array.everyi((v, i) => v === r->Array.unsafe_get(i))
    },
    () => numbers->Array.map(i => i + a->Act.get->mod(2) - b->Act.get->mod(2)),
  )
  let e = Act.computed(() => hard(c->Act.get + a->Act.get + d->Act.get->Array.unsafe_get(0), "E"))
  let f = Act.computed(() =>
    hard(
      {
        let dValue = d->Act.get->Array.unsafe_get(0)
        dValue === 0 ? dValue : b->Act.get
      },
      "F",
    )
  )
  let g = Act.computed(() =>
    c->Act.get +
    {
      let cValue = c->Act.get
      cValue === 0 ? e->Act.get->mod(2) : cValue
    } +
    d->Act.get->Array.unsafe_get(0) +
    f->Act.get
  )
  let _ = g->Act.subscribe(v => res->Array.push(hard(v, "H"))->ignore)
  let _ = g->Act.subscribe(v => res->Array.push(v)->ignore)
  let _ = f->Act.subscribe(v => res->Array.push(hard(v, "J"))->ignore)

  for i in 1 downto 0 {
    %raw("res.length = 0")
    b->Act.set(1)
    a->Act.set(1 + i * 2)
    Act.notify(.)

    a->Act.set(2 + i * 2)
    b->Act.set(2)
    Act.notify(.)

    t->Assert.is(res->Array.length, 4, ())
    t->Assert.deepEqual(res, [3198, 1601, 3195, 1598], ())
  }
})

test("throw should not broke linking", t => {
  try {
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

asyncTest("redefine act.notify", async t => {
  Act.wrapNotify(notify => {
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
