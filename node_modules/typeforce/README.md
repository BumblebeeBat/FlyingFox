# typeforce

[![build status](https://secure.travis-ci.org/dcousens/typeforce.png)](http://travis-ci.org/dcousens/typeforce)
[![Version](http://img.shields.io/npm/v/typeforce.svg)](https://www.npmjs.org/package/typeforce)

Another biased type checking solution for Javascript.

## Examples

``` javascript
var typeforce = require('typeforce')

var element = { prop: 'foo' }
var elementNumber = { prop: 2 }
var array = [element, element, elementNumber]

// supported primitives 'Array', 'Boolean', 'Buffer', 'Number', 'Object', 'String'
typeforce('Array', array)

typeforce('Number', array)
// TypeError: Expected Number, got Array

// array types
typeforce(['Object'], array)
typeforce(typeforce.arrayOf('Object'), array)

// supports recursive type templating
typeforce({ prop: 'Number' }, elementNumber)

// maybe types
typeforce('?Number', 2)
typeforce('?Number', null)
typeforce(typeforce.maybe(typeforce.Number), 2)
typeforce(typeforce.maybe(typeforce.Number), null)

// sum types
typeforce(typeforce.oneOf(['String', 'Number']))

// value types
typeforce(typeforce.value(3.14), 3.14)

// custom types
function LongString (value, strict) {
	if (!typeforce.String(value)) return false
	if (value.length !== 32) return false
	return true
}

typeforce(LongString, '00000000000000000000000000000000')
// => OK!

typeforce(LongString, 'not long enough')
// TypeError: Expected LongString, got String 'not long enough'
```

**Pro**tips:

``` javascript
// use precompiled primitives for high performance
typeforce(typeforce.Array, array)

// or just precompile a template
var type = {
	foo: 'Number',
	bar: '?String'
}

var fastType = typeforce.compile(type)
// fastType => typeforce.object({
//   foo: typeforce.Number,
//   bar: typeforce.maybe(typeforce.String)
// })

// use strictness for recursive types to enforce whitelisting properties
typeforce({
	x: 'Number'
}, { x: 1 }, true)
// OK!

typeforce({
	x: 'Number'
}, { x: 1, y: 2 }, true)
// TypeError: Unexpected property 'y' of type Number
```

## License

This library is free and open-source software released under the MIT license.

