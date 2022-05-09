export function readInt(value) {
      var n = parseInt(value, 10);

      if (typeof n === 'number' &&
          n === n &&
          n !== Infinity &&
          value !== -Infinity &&
          Math.floor(n) === n &&
          Math.abs(n) <= Number.MAX_SAFE_INTEGER)
            return n;
}

export function showForeigner(f) {
      return f +'';
}