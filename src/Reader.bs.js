// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function run(param, env) {
  return Curry._1(param[0], env);
}

function $$return(a) {
  return /* Reader */[(function (_env) {
              return a;
            })];
}

function ask(param) {
  return /* Reader */[(function (env) {
              return env;
            })];
}

function local(f, r) {
  return /* Reader */[(function (env) {
              return run(r, Curry._1(f, env));
            })];
}

function map(f, r) {
  return /* Reader */[(function (env) {
              return Curry._1(f, run(r, env));
            })];
}

function bind(f, r) {
  return /* Reader */[(function (env) {
              return run(Curry._1(f, run(r, env)), env);
            })];
}

function $great$great$eq(r, f) {
  return bind(f, r);
}

exports.run = run;
exports.$$return = $$return;
exports.ask = ask;
exports.local = local;
exports.map = map;
exports.bind = bind;
exports.$great$great$eq = $great$great$eq;
/* No side effect */
