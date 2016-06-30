var assert = require('assert')
var benchmark = require('benchmark')
benchmark.options.minTime = 1

var local = require('../')
var npm = require('typeforce')

function CustomType () { return 'ensure non-greedy match'.toUpperCase() }
var CUSTOM_TYPES = {
  'Buffer': new Buffer(1),
  'CustomType': new CustomType(),
  'Function': function () {}
}

var fixtures = require('../test/fixtures').valid
// var fixtures = [
//   { 'type': '?Number', 'value': null },
//   { 'type': [ '?Number' ], 'value': [ 1, null ] },
//   { 'type': [ { 'a': 'Number' } ], 'value': [ { 'a': 1 }, { 'a': 2 } ] },
//   { 'type': [ { 'a': '?Number' } ], 'value': [ { 'a': 1 }, { 'a': null } ] }
// ]

fixtures.forEach(function (f) {
  var suite = new benchmark.Suite()
  var tdescription = JSON.stringify(f.type)

  var actualValue = f.custom ? CUSTOM_TYPES[f.custom] : f.value
  var ctype = local.compile(f.type)

  if (f.exception) {
    assert.throws(function () { local(f.type, actualValue, f.strict) }, new RegExp(f.exception))
    assert.throws(function () { npm(f.type, actualValue, f.strict) }, new RegExp(f.exception))
    assert.throws(function () { local(ctype, actualValue, f.strict) }, new RegExp(f.exception))
    assert.throws(function () { npm(ctype, actualValue, f.strict) }, new RegExp(f.exception))

    suite.add('local(e)#' + tdescription, function () { try { local(f.type, actualValue, f.strict) } catch (e) {} })
    suite.add('  npm(e)#' + tdescription, function () { try { npm(f.type, actualValue, f.strict) } catch (e) {} })
    suite.add('local(c, e)#' + tdescription, function () { try { local(ctype, actualValue, f.strict) } catch (e) {} })
    suite.add('  npm(c, e)#' + tdescription, function () { try { npm(ctype, actualValue, f.strict) } catch (e) {} })
  } else {
    local(ctype, actualValue, f.strict)
    npm(ctype, actualValue, f.strict)

    suite.add('local#' + tdescription, function () { local(f.type, actualValue, f.strict) })
    suite.add('  npm#' + tdescription, function () { npm(f.type, actualValue, f.strict) })
    suite.add('local(c)#' + tdescription, function () { local(ctype, actualValue, f.strict) })
    suite.add('  npm(c)#' + tdescription, function () { npm(ctype, actualValue, f.strict) })
  }

  // after each cycle
  suite.on('cycle', function (event) {
    console.log('*', String(event.target))
  })

  // other handling
  suite.on('complete', function () {
    console.log('\n> Fastest is' + (' ' + this.filter('fastest').pluck('name').join(' | ')).replace(/\s+/, ' ') + '\n')
  })

  suite.on('error', function (event) {
    throw event.target.error
  })

  suite.run()
})
