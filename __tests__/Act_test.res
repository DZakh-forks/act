module Promise = Js.Promise2
module Global = Js.Global
open Ava

test("[ReScript] read outside effect stale computed", t => {
  let a = Act.make(0)
  let b = () => a->Act.get

  t->Assert.is(b(), 0, ())

  a->Act.set(1)
  t->Assert.is(b(), 1, ())
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
