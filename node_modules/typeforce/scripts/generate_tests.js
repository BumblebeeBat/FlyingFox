var typeforce = require('../')
var TYPES = require('../test/types')
var VALUES = require('../test/values')

var TYPES2 = [
  'Array',
  'Boolean',
  'Buffer',
  'Function',
  'Null',
  'Number',
  'Object',
  'String',
  '?Number',
  [ '?Number' ],
  [ 'Number' ],
  [ { a: 'Number' } ],
  { a: 'Number' },
  { a: { b: 'Number' } },
  { a: { b: { c: '?Number' } } },
  { a: { b: { c: 'Number' } } },

  // these will resolve to typeforce.value(...)
  undefined,
  null,
  true,
  false,
  0
]

var VALUES2 = [
  '',
  'foobar',
  0,
  1,
  [],
  [0],
  ['foobar'],
  [{ a: 0 }],
  [null],
  false,
  true,
  undefined,
  null,
  {},
  { a: null },
  { a: 0 },
  { a: 0, b: 0 },
  { b: 0 },
  { a: { b: 0 } },
  { a: { b: null } },
  { a: { b: { c: 0 } } },
  { a: { b: { c: null } } },
  { a: { b: { c: 0, d: 0 } } },
  { a: 'foo', b: 'bar' },
  { a: 'foo', b: { c: 'bar' } }
]

var fixtures = {
  valid: [],
  invalid: []
}

TYPES2.concat(Object.keys(TYPES)).forEach(function (type) {
  VALUES2.concat(Object.keys(VALUES)).forEach(function (value) {
    var f = {}
    var atype, avalue

    if (TYPES[type]) {
      f.typeId = type
      atype = TYPES[type]
    } else {
      f.type = type
      atype = type
    }

    if (VALUES[value]) {
      f.valueId = value
      avalue = VALUES[value]
    } else {
      f.value = value
      avalue = value
    }

    try {
      typeforce(atype, avalue, true)
      fixtures.valid.push(f)
    } catch (e) {
      try {
        typeforce(atype, avalue, false)

        fixtures.valid.push(f)
        fixtures.invalid.push(Object.assign({
          exception: e.message,
          strict: true
        }, f))
      } catch (e) {
        fixtures.invalid.push(Object.assign({
          exception: e.message
        }, f))
      }
    }
  })
})

console.log(JSON.stringify(fixtures, null, 2))
