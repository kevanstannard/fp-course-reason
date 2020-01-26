// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Reader$FpCourseReason = require("./Reader.bs.js");

var hi = Reader$FpCourseReason.$great$great$eq(Reader$FpCourseReason.ask(/* () */0), (function (name) {
        return Reader$FpCourseReason.$$return("Hi " + name);
      }));

var bye = Reader$FpCourseReason.$great$great$eq(Reader$FpCourseReason.ask(/* () */0), (function (name) {
        return Reader$FpCourseReason.$$return("Bye " + name);
      }));

var hiBye = Reader$FpCourseReason.$great$great$eq(hi, (function (h) {
        return Reader$FpCourseReason.$great$great$eq(bye, (function (b) {
                      return Reader$FpCourseReason.$$return(/* tuple */[
                                  h,
                                  b
                                ]);
                    }));
      }));

function format(param) {
  return Reader$FpCourseReason.map((function (param) {
                return param[0] + (", " + param[1]);
              }), param);
}

function makeSuper(param) {
  return Reader$FpCourseReason.local((function (env) {
                return "Super " + env;
              }), param);
}

var r = makeSuper(format(hiBye));

function start(param) {
  var result = Reader$FpCourseReason.run(r, "Joe");
  console.log(result);
  return /* () */0;
}

var WithReader = {
  hi: hi,
  bye: bye,
  hiBye: hiBye,
  format: format,
  makeSuper: makeSuper,
  r: r,
  start: start
};

function hi$1(name) {
  return "Hi " + name;
}

function bye$1(name) {
  return "Bye " + name;
}

function hiBye$1(name) {
  var h = "Hi " + name;
  var b = "Bye " + name;
  return /* tuple */[
          h,
          b
        ];
}

function format$1(param) {
  return param[0] + (", " + param[1]);
}

function makeSuper$1(env) {
  return "Super " + env;
}

function r$1(env) {
  return format$1(hiBye$1("Super " + env));
}

function start$1(param) {
  var result = format$1(hiBye$1("Super Joe"));
  console.log(result);
  return /* () */0;
}

var WithoutReader = {
  hi: hi$1,
  bye: bye$1,
  hiBye: hiBye$1,
  format: format$1,
  makeSuper: makeSuper$1,
  r: r$1,
  start: start$1
};

start(/* () */0);

start$1(/* () */0);

exports.WithReader = WithReader;
exports.WithoutReader = WithoutReader;
/* hi Not a pure module */