'use strict';

const fix =
  fn =>
  (...args) =>
    fn(fix(fn), ...args);

const curry = fn =>
  fix((self, ...args) =>
    args.length >= fn.length ? fn(...args) : (...next) => self(...args, ...next)
  );

const uncurry =
  fn =>
  (...args) =>
    args.reduce((f, arg) => f(arg), fn);

// Note to reader: Add your solutions to this file
export const volumeFn = (a, b, c) => a * b * c;

export const volumeArrow = curry(volumeFn);

const reduce = curry((fn, acc, xs) => xs.reduce(fn, acc));
const prop = (k, o) => o[k];

export const cumulativeSumsComplex = xs =>
  prop(
    'sums',
    reduce(
      ({ real, imag, sums }, { real: r, imag: i }) => {
        real += r;
        imag += i;
        sums.push({ real, imag });
        return { sums, real, imag };
      },
      { sums: [], real: 0, imag: 0 },
      xs
    )
  );
