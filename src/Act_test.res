@module("uvu") external test: (string, unit => unit) => unit = "test"
@module("uvu") @scope("test") external testRun: unit => unit = "run"
module Assert = {
  @module("uvu/assert") external is: ('a, 'a) => unit = "is"
}

test("[ReScript] read outside effect stale computed", () => {
  let a = Act.make(0)
  let b = () => a->Act.get

  Assert.is(b(), 0)

  a->Act.set(1)
  Assert.is(b(), 1)
})

testRun()
