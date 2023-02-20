'use strict';

const fix =
  fn =>
  (...args) =>
    fn(fix(fn), ...args);

const curry = fn =>
  fix((self, ...args) =>
    args.length >= fn.length ? fn(...args) : (...next) => self(...args, ...next)
  );

const apply = (f, ...args) => f(...args);
const reduce = curry((fn, acc, xs) => xs.reduce(fn, acc));

const uncurry =
  fn =>
  (...args) =>
    reduce(apply, fn, args);

const prop = (k, o) => o[k];

// Note to reader: Add your solutions to this file
export const volumeFn = (a, b, c) => a * b * c;

export const volumeArrow = curry(volumeFn);

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

// (-b +/- sqrt (b^2 - 4ac)) / 2a
export const quadraticRootsJson = ({ a, b, c }) => {
  const twoA = 2 * a;
  const radicand = b ** 2 - 4 * a * c;
  const sqrt = Math.sqrt(Math.abs(radicand));
  if (radicand >= 0) {
    return [
      { real: (-b + sqrt) / twoA, imag: 0 },
      {
        real: (-b - sqrt) / twoA,
        imag: 0
      }
    ];
  } else {
    const real = -b / twoA;
    const imag = sqrt / twoA;
    return [
      { real, imag },
      { real, imag: -imag }
    ];
  }
};

export const quadraticRootsImpl = pair => quad =>
  uncurry(pair)(...quadraticRootsJson(quad));

export const toMaybeImpl = just => nothing => x =>
  x === undefined ? nothing : just(x);

export const valuesOfMapJson = json => [...new Set(new Map(json).values())];
