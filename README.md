**Act** is a reactive library that is incredibly efficient in terms of speed, size and consistency. [Here is](https://perf.js.hyoo.ru/#!bench=9h2as6_u0mfnn) a big benchmark comparing all popular state managers, which Act passes with flying colors. The size is only [0.4KB gzipped](https://bundlejs.com/?q=%40artalar%2Fact)!

I built it for fun, but you are free to use it in your projects. I'm open to bug reports and suggestions.

> See also: [how is Act so small and so fast?](#how-is-it-so-small-and-so-fast)

## Installation

```sh
npm i @artalar/act
```

## Usage

Pass initial value to `act` function to create mutable observer or pass a callback to create a computed observer. You could mutate observer by call it with a new value. All observers are lazy and recalculates only when has a subscribtion. You could read acts (lets call it "act" / "acts") by call it as a function. Act reading inside a computer is reactive.

All updates are butch and will be applied in the next microtask. So you can update multiple acts synchronously and all computed will callculate and subscribers will be notified only once. You could redefine this behavior by [overriding `act.notify` method](#sync-batch).

> See [React example below](#react-example).

### Basic example

You could use Act with React and Svelte without bindings!

[![Edit @artalar/act](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/artalar-act-9wz836?file=/src/index.ts)

```ts
import { act } from '@artalar/act'

// create mutable reactive value reference
const counterAct = act(0)
// create computed reactive value reference, use other acts here in any conditions
const isOddAct = act(() => Boolean(counterAct() % 2))

// kind a actions
const set = (n: number) => counterAct(n)
const add = (n: number) => counterAct(counterAct() + n)
const inc = () => add(1)

// subscribe
const un = isOddAct.subscribe((v) => console.log('isOdd', v))
// 'isOdd false'

;(async () => {
  set(0)
  await 0
  // nothing

  inc()
  await 0
  // 'isOdd true'
  add(2)
  await 0
  // nothing
  inc()
  await 0
  // 'isOdd false'

  inc()
  inc()

  await 0
  // nothing

  inc()
  inc()
  inc()

  await 0
  // 'isOdd true'

  // unsubscribe
  un()
  inc()
  await 0
  // nothing
})()
```

### Filter example

The computed act accepts an equality check function as the second argument and allows you to filter updates with `shallowEqual` etc.

```ts
const filterAct = act('')
const listAct = act([])
const listFilteredAct = act(
  () => {
    const filter = filterAct()
    return listAct().filter((text) => text.includes(filter))
  },
  (prev, next) => isShallowEqual(prev, next),
)
```

### Conditional branches example

Act's proudest feature is conditional branches optimization. Your conditions and the usage of other acts in them, no matter how complex, will be optimized in the most efficient way at the minimal cost.

```ts
const isAdminAct = act(false)
const listAct = act([])
const filterAct = act('')
const listFilteredAct = act(
  () => listAct().filterAct((text) => text.includes(filterAct())),
  isShallowEqual,
)
export const listViewAct = act(() =>
  isAdminAct() ? listFilteredAct() : listAct(),
)
```

### Dynamic acts example

This snippet shows how you can use (subscribe to) acts inside loops and create (and delete) acts dynamically. This could be useful for optimizing your data updates and subscriptions.

> See [example dynamic list in React](#react-example-dynamic-list).

```ts
const listAct = act([])
const sumAct = act(() =>
  listAct().reduce((acc, counterAct) => acc + counterAct(), 0),
)
const add = () => listAct([...listAct(), act(0)])
```

### React example

Here comes the magic... You don't need an adapter to use Act in React! The built-in [useSyncExternalStore](https://beta.reactjs.org/reference/react/useSyncExternalStore) is the only thing you need.

[![Edit @artalar/act react](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/artalar-act-react-vyqch1?fontsize=14&hidenavigation=1&theme=dark)

```ts
const counterAct = act(0)
const inc = () => counterAct(counterAct() + 1)

export default function App() {
  const counter = useSyncExternalStore(counterAct.subscribe, counterAct)

  return <button onClick={inc}>{counter}</button>
}
```

### React example dynamic list

This example shows how you can share state between components and optimize rerenders with a couple of lines.

[![Edit @artalar/act react](https://codesandbox.io/static/img/play-codesandbox.svg)](https://codesandbox.io/s/artalar-act-react-list-vesmct?file=/src/App.tsx)

### Svelte

Act provides a compatible subscription interface, so you can use the `$` prefix to subscribe to an act.

[REPL example](https://svelte.dev/repl/66d2d612134c46d3b3f5a0b933d2c200?version=3.55.0)

### Sync batch

You could specify your own batch function by redefining `act.notify`. By default `act.notify` scheduled to the next tick, but you could handle it and delay for a more time or call it synchronously to call subscribers immediately.

```ts
const { notify } = act
act.notify = () => requestAnimationFrame(notify)
```

## How is it so small and so fast?

I have spent half a decade researching reactive programming and prototyping. The most production-ready and feature-rich result of my work is [reatom.dev](https://www.reatom.dev/). Under the hood Reatom uses topological sorting on top of immutable graph features, to achieve things like DI and lifecycle hooks. But does it apply to each and every use case? Nope. So I decided to create a lightweight version of Reatom with a simpler api, lower cost and smaller size.

Sooo, how does it work? One of the challenges in reactive programming is optimizing node combination, which can cause [glitches](https://en.wikipedia.org/wiki/Reactive_programming#Glitches). However, conditional usage of computed dependencies is the hardest and most painful thing, as there is a lot of corner cases to consider if you want conditions of any kind to be supported in the most optimal way.

For example, when you need to unsubscribe from all unused dependencies, how do you know which of them are used and which are not? Basically, there is a hook working under the hood to link the parent and the child of a computation. And when the dependencies used in your computations are the same every time, the hook understands it and doesn't resubscribe to them. But what if you use the same dependency several times? Or, when you stop using a dependency at the top level of your computations, but keep using other dependencies at lower levels, how to understand whether the lower dependencies are new or were used before?

All these cases are managed with a complex cache invalidation policy which has a significant performance and memory cost.

But... We already use a lot of memory for cache and CPU for its invalidation? So, maybe that overhead is not worth it and the whole recalculation of all dependency graph could be cheaper? Is it? Well, this library answers **YES**.

Act does not use bidirectional links in dependency graphs and doesn't need to invalidate them. The only connection built from scratch on each update is "subscriber <- (any number of computed nodes without knowledge of each other) <- value setter (`ActValue` type)". This is super cheap.

But there is one limitation: as we don't have cross-links and invalidation states, if we want to read an act without subsciption, we need to run all invalidation logic each time. And there is another performance. Because dependency setters need to know about each subscriber, each subscriber should traverse the whole graph every time and if you have one complex act with many subscribers it would not be perfectly optimal. The good news are: 1) this is a rare case; 2) the whole graph traversal is probably cheap thanks to JIT.

However, [Reatom](https://www.reatom.dev/) combines both approaches and will optimize all your computations in the most complex cases, allowing you to inspect immutable snaphots of any update. If you need something more feature-rich, have a look at it.
