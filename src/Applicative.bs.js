// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Listz$FpCourseReason = require("./Listz.bs.js");
var Functor$FpCourseReason = require("./Functor.bs.js");

var map = Functor$FpCourseReason.ExactlyOneFunctor.map;

function pure(a) {
  return /* ExactlyOne */[a];
}

function apply(f, a) {
  return /* ExactlyOne */[Curry._1(f[0], a[0])];
}

var ExactlyOneApplicative = {
  map: map,
  pure: pure,
  apply: apply
};

var map$1 = Functor$FpCourseReason.ListzFunctor.map;

function pure$1(a) {
  return /* :: */[
          a,
          /* [] */0
        ];
}

function apply$1(fz, az) {
  var mapFn = function (f) {
    return Curry._2(map$1, f, az);
  };
  return Listz$FpCourseReason.flatten(Curry._2(map$1, mapFn, fz));
}

var ListzApplicative = {
  map: map$1,
  pure: pure$1,
  apply: apply$1
};

var map$2 = Functor$FpCourseReason.OptionFunctor.map;

function pure$2(a) {
  return Caml_option.some(a);
}

function apply$2(fOpt, aOpt) {
  if (fOpt !== undefined && aOpt !== undefined) {
    return Caml_option.some(Curry._1(fOpt, Caml_option.valFromOption(aOpt)));
  }
  
}

var OptionApplicative = {
  map: map$2,
  pure: pure$2,
  apply: apply$2
};

function MakeFunctionApplicative(TYPE) {
  var FunctionFunctor = Functor$FpCourseReason.MakeFunctionFunctor(TYPE);
  var map = FunctionFunctor.Functor.map;
  var pure = function (a, _t) {
    return a;
  };
  var apply = function (tab, ta, t) {
    return Curry._2(tab, t, Curry._1(ta, t));
  };
  var Applicative = {
    map: map,
    pure: pure,
    apply: apply
  };
  return {
          FunctionFunctor: FunctionFunctor,
          Applicative: Applicative
        };
}

function MakeApplicativeUtils(Applicative) {
  var $less$$great = Applicative.map;
  var $less$star$great = Applicative.apply;
  var lift2 = function (abc, ta, tb) {
    return Curry._2($less$star$great, Curry._2($less$$great, abc, ta), tb);
  };
  var lift2$prime = function (abc, ta, tb) {
    var tbc = Curry._2(Applicative.map, abc, ta);
    return Curry._2(Applicative.apply, tbc, tb);
  };
  var lift3 = function (abcd, ta, tb, tc) {
    return Curry._2($less$star$great, lift2(abcd, ta, tb), tc);
  };
  var lift4 = function (abcde, ta, tb, tc, td) {
    return Curry._2($less$star$great, lift3(abcde, ta, tb, tc), td);
  };
  var lift0 = function (a) {
    return Curry._1(Applicative.pure, a);
  };
  var lift1 = function (ab, ta) {
    return Curry._2($less$star$great, Curry._1(Applicative.pure, ab), ta);
  };
  var rightApply = function (ta, tb) {
    var f = function (param, b) {
      return b;
    };
    return lift2(f, ta, tb);
  };
  var leftApply = function (tb, ta) {
    var f = function (b, param) {
      return b;
    };
    return lift2(f, tb, ta);
  };
  var sequence = function (lta) {
    return Belt_List.reduce(lta, Curry._1(Applicative.pure, /* [] */0), (function (tla, ta) {
                  return lift2((function (la, a) {
                                return Belt_List.concat(la, /* :: */[
                                            a,
                                            /* [] */0
                                          ]);
                              }), tla, ta);
                }));
  };
  return {
          $less$$great: $less$$great,
          $less$star$great: $less$star$great,
          lift2: lift2,
          lift2$prime: lift2$prime,
          lift3: lift3,
          lift4: lift4,
          lift0: lift0,
          lift1: lift1,
          rightApply: rightApply,
          $star$great: rightApply,
          leftApply: leftApply,
          $less$star: leftApply,
          sequence: sequence
        };
}

exports.ExactlyOneApplicative = ExactlyOneApplicative;
exports.ListzApplicative = ListzApplicative;
exports.OptionApplicative = OptionApplicative;
exports.MakeFunctionApplicative = MakeFunctionApplicative;
exports.MakeApplicativeUtils = MakeApplicativeUtils;
/* Listz-FpCourseReason Not a pure module */
