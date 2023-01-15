// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Act = require("../src/Act.bs.js");
var Ava = require("ava").default;
var Js_exn = require("rescript/lib/js/js_exn.js");

Ava("[ReScript] read outside effect stale computed", (function (t) {
        var a = Act.make(0);
        t.is(a.g(), 0, undefined);
        a.s(1);
        t.is(a.g(), 1, undefined);
      }));

Ava("[ReScript] https://perf.js.hyoo.ru/#!bench=9h2as6_u0mfnn", (function (t) {
        var res = [];
        var a = Act.make(0);
        var b = Act.make(0);
        var c = Act.computed(function (param) {
              return a.g() % 2 + b.g() % 2 | 0;
            });
        Act.subscribe(c, (function (v) {
                res.push(v);
              }));
        t.deepEqual(res, [0], undefined);
      }));

Ava("throw should not broke linking", (function (t) {
        try {
          Act.subscribe(Act.computed(function (param) {
                    return Js_exn.raiseError("Foo");
                  }), (function (param) {
                  
                }));
        }
        catch (exn){
          
        }
        var a = Act.make(0);
        var b = Act.computed(function (param) {
              return a.g();
            });
        var c = Act.computed(function (param) {
              return a.g();
            });
        Act.subscribe(c, (function (param) {
                
              }));
        a.s(1);
        t.deepEqual([
              a.g(),
              b.g(),
              c.g()
            ], [
              1,
              1,
              1
            ], undefined);
      }));

Ava("[ReScript] redefine act.notify", (async function (t) {
        var notify = Act.getNotify(undefined);
        Act.setNotify(function () {
              setTimeout((function (param) {
                      notify();
                    }), 0);
            });
        var a = Act.make(0);
        var callsRef = {
          contents: 0
        };
        Act.subscribe(a, (function (param) {
                callsRef.contents = callsRef.contents + 1 | 0;
              }));
        t.is(callsRef.contents, 1, undefined);
        a.s(123);
        await Promise.resolve(undefined);
        t.is(callsRef.contents, 1, undefined);
        await new Promise((function (resolve, param) {
                setTimeout((function (param) {
                        resolve(undefined);
                      }), 0);
              }));
        t.is(callsRef.contents, 2, undefined);
      }));

/*  Not a pure module */
