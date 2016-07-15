// BERT-NODE
// Arnaud Wetzel, Inspired by bert.js of 2009 Rusty Klophaus (@rklophaus)
// Rewrite it to
// - code and decode from node-js Buffer objects
// - handle erlang 17 maps
// - binary type is Buffer
//
// References: http://www.erlang.org/doc/apps/erts/erl_ext_dist.html#8

function BertClass() {
    this.BERT_START = 131;
    this.SMALL_ATOM = 115;
    this.ATOM = 100;
    this.BINARY = 109;
    this.SMALL_INTEGER = 97;
    this.INTEGER = 98;
    this.SMALL_BIG = 110;
    this.LARGE_BIG = 111;
    this.FLOAT = 99;
    this.STRING = 107;
    this.LIST = 108;
    this.SMALL_TUPLE = 104;
    this.LARGE_TUPLE = 105;
    this.NIL = 106;
    this.MAP = 116;
    this.NEW_FLOAT = 70;
    this.ZERO = 0;

    this.ELIXIR = 0;
    this.ERLANG = 1;

    this.all_binaries_as_string = false;
    this.map_key_as_atom = true;
    this.decode_undefined_values = true;
    this.convention = this.ELIXIR;

    this.output_buffer = new Buffer(10000000);
    this.output_buffer[0] = this.BERT_START;
}

function BertAtom(Obj) {
    this.type = "Atom";
    this.value = Obj;
    this.toString = function () {
	return Obj;
    };
}

function BertTuple(Arr) {
    this.type = "Tuple";
    this.length = Arr.length;
    this.value = Arr;
    for (var i = 0; i < Arr.length; i++) {
	this[i] = Arr[i];
    }
    this.toString = function () {
	var i, s = "";
	for (i = 0; i < this.length; i++) {
	    if (s !== "") {
		s += ", ";
	    }
	    s += this[i].toString();
	}

	return "{" + s + "}";
    };
}



// - INTERFACE -
BertClass.prototype.encode = function (Obj,nocopy) {
    if (nocopy === undefined) var nocopy = false;
    var tail_buffer = this.encode_inner(Obj,this.output_buffer.slice(1));
    if(tail_buffer.length == 0){
	throw new Error("Bert encode a too big term, encoding buffer overflow");
    }
    if(!nocopy){
	res = new Buffer(this.output_buffer.length - tail_buffer.length);
	this.output_buffer.copy(res,0,0,res.length);
	return res;
    }else{
	return this.output_buffer.slice(0,this.output_buffer.length - tail_buffer.length);
    }
};

BertClass.prototype.decode = function (buffer) {
    if (buffer[0] !== this.BERT_START) {
	throw ("Not a valid BERT.");
    }
    var Obj = this.decode_inner(buffer.slice(1));
    if (Obj.rest.length !== 0) {
	throw ("Invalid BERT.");
    }
    return Obj.value;
};

BertClass.prototype.atom = function (Obj) {
    return new BertAtom(Obj);
};

BertClass.prototype.tuple = function () {
    return new BertTuple(arguments);
};



// - ENCODING -

BertClass.prototype.encode_inner = function (Obj, buffer) {
    var func = 'encode_' + typeof(Obj);
    return this[func](Obj,buffer);
};

BertClass.prototype.encode_string = function (Obj, buffer) {
    if (this.convention === this.ELIXIR){
	return this.encode_binary(new Buffer(Obj), buffer);
    } else {
	buffer[0] = this.STRING;
	buffer.writeUInt16BE(Obj.length,1);
	var len = buffer.write(Obj,3);
	return buffer.slice(3+len);
    }
};

BertClass.prototype.encode_boolean = function (Obj, buffer) {
    if (Obj) {
	return this.encode_inner(this.atom("true"), buffer);
    }
    else {
	return this.encode_inner(this.atom("false"), buffer);
    }
};

BertClass.prototype.encode_number = function (Obj, buffer) {
    var s, isInteger = (Obj % 1 === 0);

    // Handle floats...
    if (!isInteger) {
	return this.encode_float(Obj, buffer);
    }

    // Small int...
    if (isInteger && Obj >= 0 && Obj < 256) {
	buffer[0] = this.SMALL_INTEGER;
	buffer.writeUInt8(Obj, 1);
	return buffer.slice(2);
    }

    // 4 byte int...
    if (isInteger && Obj >= -134217728 && Obj <= 134217727) {
	buffer[0] = this.INTEGER;
	buffer.writeInt32BE(Obj, 1);
	return buffer.slice(5);
    }

    // Bignum...
    var num_buffer = new Buffer(buffer.length);
    if (Obj < 0) {
	Obj *= -1;
	num_buffer[0] = 1;
    } else {
	num_buffer[0] = 0;
    }
    var offset = 1;
    while (Obj !== 0) {
	num_buffer[offset] = Obj % 256;
	Obj = Math.floor(Obj / 256);
	offset++;
    }
    if (offset < 256) {
	buffer[0] = this.SMALL_BIG;
	buffer.writeUInt8(offset - 1, 1);
	num_buffer.copy(buffer,2,0,offset);
	return buffer.slice(2 + offset);
    } else {
	buffer[0] = this.LARGE_BIG;
	buffer.writeUInt32BE(offset - 1, 1);
	num_buffer.copy(buffer,5,0,offset);
	return buffer.slice(5 + offset);
    }
};

BertClass.prototype.encode_float = function (Obj, buffer) {
    // float...
    buffer[0] = this.NEW_FLOAT;
    buffer.writeDoubleBE(Obj,1);
    return buffer.slice(9);
};

BertClass.prototype.encode_object = function (Obj, buffer) {
    // Check if it's an atom, binary, or tuple...
    if (Obj === null){
	var undefined_atom = (this.convention === this.ELIXIR) ? "nil" : "undefined";
	return this.encode_inner(this.atom(undefined_atom),buffer);
    }
    if (Obj instanceof Buffer) {
	return this.encode_binary(Obj,buffer);
    }
    if (Obj instanceof Array) {
	return this.encode_array(Obj,buffer);
    }
    if (Obj.type === "Atom") {
	return this.encode_atom(Obj,buffer);
    }
    if (Obj.type === "Tuple") {
	return this.encode_tuple(Obj,buffer);
    }
    // Treat the object as an associative array...
    return this.encode_map(Obj,buffer);
};

BertClass.prototype.encode_atom = function (Obj, buffer) {
    buffer[0] = this.ATOM;
    buffer.writeUInt16BE(Obj.value.length,1);
    var len = buffer.write(Obj.value,3);
    return buffer.slice(3+len);
};

BertClass.prototype.encode_binary = function (Obj, buffer) {
    buffer[0] = this.BINARY;
    buffer.writeUInt32BE(Obj.length,1);
    Obj.copy(buffer,5);
    return buffer.slice(5 + Obj.length);
};

// undefined is null
BertClass.prototype.encode_undefined = function (Obj, buffer) {
    return this.encode_inner(null,buffer)
}

BertClass.prototype.encode_tuple = function (Obj, buffer) {
    var i;
    if (Obj.length < 256) {
	buffer[0] = this.SMALL_TUPLE;
	buffer.writeUInt8(Obj.length,1);
	buffer = buffer.slice(2);
    } else {
	buffer[0] = this.LARGE_TUPLE;
	buffer.writeUInt32BE(Obj.length,1);
	buffer = buffer.slice(5);
    }
    for (i = 0; i < Obj.length; i++) {
	buffer = this.encode_inner(Obj[i],buffer);
    }
    return buffer;
};

BertClass.prototype.encode_array = function (Obj, buffer) {
    if (Obj.length == 0){
	buffer[0] = this.NIL;
	return buffer.slice(1);
    }
    buffer[0] = this.LIST;
    buffer.writeUInt32BE(Obj.length,1);
    buffer = buffer.slice(5);
    var i;
    for (i = 0; i < Obj.length; i++) {
	buffer = this.encode_inner(Obj[i],buffer);
    }
    buffer[0] = this.NIL;
    return buffer.slice(1);
};

BertClass.prototype.encode_map = function (Obj, buffer) {
    var keys = Object.keys(Obj);
    buffer[0] = this.MAP;
    buffer.writeUInt32BE(keys.length, 1);
    buffer = buffer.slice(5);
    var i;
    for (i = 0; i < keys.length; i++) {
	key = (this.map_key_as_atom) ? this.atom(keys[i]) : keys[i];
	buffer = this.encode_inner(key,buffer);
	buffer = this.encode_inner(Obj[keys[i]],buffer);
    }
    return buffer;
};



// - DECODING -

BertClass.prototype.decode_inner = function (buffer) {
    var Type = buffer[0];
    buffer = buffer.slice(1);
    switch (Type) {
    case this.SMALL_ATOM:
	return this.decode_atom(buffer, 1);
    case this.ATOM:
	return this.decode_atom(buffer, 2);
    case this.BINARY:
	return this.decode_binary(buffer);
    case this.SMALL_INTEGER:
	return this.decode_integer(buffer, 1, true);
    case this.INTEGER:
	return this.decode_integer(buffer, 4);
    case this.SMALL_BIG:
	return this.decode_big(buffer, 1);
    case this.LARGE_BIG:
	return this.decode_big(buffer, 4);
    case this.FLOAT:
	return this.decode_float(buffer);
    case this.NEW_FLOAT:
	return this.decode_new_float(buffer);
    case this.STRING:
	return this.decode_string(buffer);
    case this.LIST:
	return this.decode_list(buffer);
    case this.SMALL_TUPLE:
	return this.decode_tuple(buffer, 1);
    case this.LARGE_TUPLE:
	return this.decode_large_tuple(buffer, 4);
    case this.NIL:
	return this.decode_nil(buffer);
    case this.MAP:
	return this.decode_map(buffer);
    default:
	throw ("Unexpected BERT type: " + Type);
    }
};

BertClass.prototype.decode_atom = function (buffer, Count) {
    var Size, Value;
    Size = this.bytes_to_int(buffer, Count);
    buffer = buffer.slice(Count);
    Value = buffer.toString('utf8',0, Size);
    if (Value === "true") {
	Value = true;
    }
    else if (Value === "false") {
	Value = false;
    }
    else if (this.decode_undefined_values && this.convention === this.ELIXIR && Value === "nil") {
	Value = null;
    }
    else if (this.decode_undefined_values && this.convention === this.ERLANG && Value === "undefined") {
	Value = null;
    }
    else{
	Value = this.atom(Value);
    }
    return {
	value: Value,
	rest:  buffer.slice(Size)
    };
};

BertClass.prototype.decode_binary = function (buffer) {
    var Size = this.bytes_to_int(buffer, 4);
    buffer = buffer.slice(4);
    var bin = new Buffer(Size);
    buffer.copy(bin,0,0,Size);
    return {
	value: (this.convention === this.ELIXIR && this.all_binaries_as_string) ? bin.toString() : bin,
	rest:  buffer.slice(Size)
    };
};

BertClass.prototype.decode_integer = function (buffer, Count, unsigned) {
    return {
	value: this.bytes_to_int(buffer, Count, unsigned),
	rest:  buffer.slice(Count)
    };
};

BertClass.prototype.decode_big = function (buffer, Count) {
    var Size = this.bytes_to_int(buffer, Count);
    buffer = buffer.slice(Count);

    var isNegative, i, n, Num = 0;
    isNegative = (buffer[0] === 1);
    buffer = buffer.slice(1);
    for (i = Size - 1; i >= 0; i--) {
	n = buffer[i];
	if (Num === 0) { Num = n; }
	else { Num = Num * 256 + n; }
    }
    if (isNegative) { Num = Num * -1 }

    return {
	value : Num,
	rest: buffer.slice(Size)
    };
};

BertClass.prototype.decode_float = function (buffer) {
    var Size = 31;
    return {
	value: parseFloat(buffer.toString('utf8',0,Size)),
	rest: buffer.slice(Size)
    };
};

BertClass.prototype.decode_new_float = function (buffer) {
    return {
	value: buffer.readDoubleBE(0),
	rest: buffer.slice(8)
    };
};

BertClass.prototype.decode_string = function (buffer) {
    var Size = this.bytes_to_int(buffer, 2);
    buffer = buffer.slice(2);
    return {
	value: buffer.toString('utf8',0,Size),
	rest:  buffer.slice(Size)
    };
};

BertClass.prototype.decode_list = function (buffer) {
    var Size, i, El, LastChar, Arr = [];
    Size = this.bytes_to_int(buffer, 4);
    buffer = buffer.slice(4);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(buffer);
	Arr.push(El.value);
	buffer = El.rest;
    }
    LastChar = buffer[0];
    if (LastChar !== this.NIL) {
	throw ("List does not end with NIL!");
    }
    buffer = buffer.slice(1);
    return {
	value: Arr,
	rest: buffer
    };
};

BertClass.prototype.decode_map = function (buffer) {
    var Size, i, El, Key, Value, Map = {};
    Size = this.bytes_to_int(buffer, 4);
    buffer = buffer.slice(4);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(buffer);
	Key = El.value;
	El = this.decode_inner(El.rest);
	Value = El.value;
	Map[Key] = Value;
	buffer = El.rest;
    }
    return {
	value: Map,
	rest: buffer
    };
};

BertClass.prototype.decode_tuple = function (buffer, Count) {
    var Size, i, El, Arr = [];
    Size = this.bytes_to_int(buffer, Count);
    buffer = buffer.slice(Count);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(buffer);
	Arr.push(El.value);
	buffer = El.rest;
    }
    return {
	value: this.tuple.apply(this,Arr),
	rest: buffer
    };
};

BertClass.prototype.decode_nil = function (buffer) {
    // nil is an empty list
    return {
	value: [],
	rest: buffer
    };
};

// Read a big-endian encoded integer from the first Length bytes
// of the supplied string.
BertClass.prototype.bytes_to_int = function (buffer, Length, unsigned) {
    switch (Length) {
    case 1:
	return unsigned ? buffer.readUInt8(0, true) : buffer.readInt8(0, true);
    case 2:
	return unsigned ? buffer.readUInt16BE(0, true) : buffer.readInt16BE(0, true);
    case 4:
	return unsigned ? buffer.readUInt32BE(0, true) : buffer.readInt32BE(0, true);
    }
};

// - TESTING -

// Pretty Print a byte-string in Erlang binary form.
BertClass.prototype.pp_bytes = function (Bin) {
    var i, s = "";
    for (i = 0; i < Bin.length; i++) {
	if (s !== "") {
	    s += ",";
	}
	s += "" + Bin[i];
    }
    return "<<" + s + ">>";
};

// Pretty Print a JS object in Erlang term form.
BertClass.prototype.pp_term = function (Obj) {
    return Obj.toString();
};

BertClass.prototype.binary_to_list = function (Str){
    var ret = [];
    for (var i = 0; i < Str.length; i++)
	ret.push(Str[i]);
    return ret;
};

module.exports = new BertClass();
