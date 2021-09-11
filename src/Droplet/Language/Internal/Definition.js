exports.readInt = function (value) {
      var n = parseInt(value, 10);

      if (typeof n === 'number' &&
          n === n &&
          n !== Infinity &&
          value !== -Infinity &&
          Math.floor(n) === n &&
          Math.abs(n) <= Number.MAX_SAFE_INTEGER)
            return n;
}

exports.showForeigner = function(f) {
      return f +'';
}