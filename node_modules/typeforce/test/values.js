module.exports = {
  'function': function () {},
  'emptyType': new function EmptyType () {},
  'customType': new function CustomType () { this.x = 2 },
  'buffer': new Buffer(0)
}
