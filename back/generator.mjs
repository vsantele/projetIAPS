import fs from "fs/promises"
// d = droit, v = virage
// Si d: ['d', 8,3] -> 8 cases de long, 3 positions possible
// Si d: ['d', 8,3,2] -> 8 cases de long, 3 positions possible, arbre position 2.
// si v: ['v',2, 1,2] -> 2 cases de larges, 1 case position 1, 2 cases en positions 2
// si v: ['v',3, 1,0, 2] -> 2 cases de larges, 1 case position 1,arbre position 2, 2 cases en positions 2
const board = [
  ["d", 8, 3, 0],
  ["s", 2],
  ["d", 8, 2, 0],
  ["d", 4, 3, 0],
  ["d", 3, 4, 3],
  ["s", 2],
  ["d", 8, 4, 3],
  ["d", 27, 2, 0],
  ["s", 2],
  ["d", 8, 2, 0],
  ["d", 3, 1, 0],
  ["d", 8, 2, 0],
  ["d", 5, 3, 2],
  ["s", 2],
  ["d", 4, 3, 2],
  ["d", 1, 3, 0],
]

function generator() {
  let x = 1
  let result = ""
  for (let iBoard = 0; iBoard < board.length; iBoard++) {
    const [type, ...args] = board[iBoard]
    if (type === "s") {
      x += args[0]
      continue
    }
    const [long, larg, tree] = args
    for (let i = 1; i < long; i++) {
      for (let y = 1; y <= larg; y++) {
        if (tree === y) {
          continue
        }
        result += writePredicat("chemin", x, y, x + 1, y)
        if (y < larg && tree !== y + 1) {
          result += writePredicat("chemin", x, y, x + 1, y + 1)
          result += writePredicat("voisin", x, y, x, y + 1)
        }
        if (y > 1 && tree !== y - 1) {
          result += writePredicat("chemin", x, y, x + 1, y - 1)
        }
      }
      x++
    }
    x++
    result += `=== Line ${long}, ${larg}, ${tree}}\n`
  }
  result += writePredicat("chemin", 8, 1, 9, 1)
  result += writePredicat("chemin", 8, 2, 9, 1)
  result += writePredicat("chemin", 8, 2, 9, 2)
  result += writePredicat("chemin", 8, 3, 9, 2)
  result += writePredicat("voisin", 8, 1, 8, 2)
  result += writePredicat("voisin", 8, 2, 8, 3)

  result += writePredicat("voisin", 9, 1, 9, 2, 1, 2)
  result += writePredicat("voisin", 9, 1, 9, 2, 1, 3)
  result += writePredicat("chemin", 9, 2, 9, 2, 3, 2)

  result += writePredicat("chemin", 9, 1, 10, 1, 1, 1)
  result += writePredicat("chemin", 9, 1, 10, 2, 1, 3)
  result += writePredicat("chemin", 9, 2, 10, 2, 2, 3)

  result += writePredicat("voisin", 10, 1, 10, 2, 1, 2)
  result += writePredicat("voisin", 10, 1, 10, 2, 1, 3)
  result += writePredicat("chemin", 10, 2, 10, 2, 3, 2)

  result += writePredicat("chemin", 10, 1, 11, 1, 1, 0)
  result += writePredicat("chemin", 10, 1, 11, 2, 1, 0)
  result += writePredicat("chemin", 10, 2, 11, 2, 2, 0)
  result += writePredicat("chemin", 10, 2, 11, 1, 2, 0)

  result += writePredicat("chemin", 18, 1, 19, 1)
  result += writePredicat("chemin", 18, 1, 19, 2)
  result += writePredicat("chemin", 18, 1, 19, 3)
  result += writePredicat("chemin", 18, 2, 19, 3)
  result += writePredicat("chemin", 18, 2, 19, 2)
  result += writePredicat("voisin", 18, 1, 18, 2)
  result += writePredicat("voisin", 18, 2, 18, 3)

  result += writePredicat("chemin", 22, 1, 23, 1)
  result += writePredicat("chemin", 22, 1, 23, 2)
  result += writePredicat("chemin", 22, 2, 23, 1)
  result += writePredicat("chemin", 22, 2, 23, 2)
  result += writePredicat("chemin", 22, 3, 23, 4)
  result += writePredicat("voisin", 22, 1, 22, 2)
  result += writePredicat("voisin", 22, 2, 22, 3)

  result += writePredicat("voisin", 25, 1, 25, 2)
  result += writePredicat("voisin", 25, 2, 25, 3)
  result += writePredicat("chemin", 26, 1, 27, 1, 1, 1)
  result += writePredicat("chemin", 26, 1, 27, 2, 1, 2)
  result += writePredicat("chemin", 26, 2, 27, 1, 2, 1)
  result += writePredicat("chemin", 26, 2, 27, 2, 2, 2)
  result += writePredicat("chemin", 26, 4, 26, 4, 3, 2)
  result += writePredicat("chemin", 26, 4, 26, 4, 3, 2)
  result += writePredicat("chemin", 26, 4, 27, 4, 2, 3)
  result += writePredicat("chemin", 27, 4, 27, 4, 3, 2)
  result += writePredicat("chemin", 27, 4, 28, 4, 3, 0)
  result += writePredicat("voisin", 26, 1, 26, 2, 1, 2)
  result += writePredicat("voisin", 26, 1, 26, 2, 1, 3)
  result += writePredicat("voisin", 27, 1, 27, 2, 1, 2)
  result += writePredicat("voisin", 27, 1, 27, 2, 1, 3)
  result += writePredicat("voisin", 27, 1, 28, 1, 1, 0)
  result += writePredicat("voisin", 27, 2, 28, 1, 2, 0)
  result += writePredicat("voisin", 27, 2, 28, 2, 2, 0)

  result += writePredicat("chemin", 34, 1, 35, 1)
  result += writePredicat("chemin", 34, 1, 35, 2)
  result += writePredicat("chemin", 34, 4, 35, 3)
  result += writePredicat("voisin", 35, 1, 35, 2)
  result += writePredicat("voisin", 35, 2, 35, 3)
  result += writePredicat("chemin", 35, 1, 36, 1)
  result += writePredicat("chemin", 35, 2, 36, 1)
  result += writePredicat("chemin", 35, 2, 36, 2)
  result += writePredicat("chemin", 35, 3, 36, 1)
  result += writePredicat("chemin", 35, 3, 36, 2)

  result += writePredicat("voisin", 62, 1, 62, 2)
  result += writePredicat("chemin", 62, 1, 63, 1, 0, 1)
  result += writePredicat("chemin", 62, 1, 63, 2, 0, 3)
  result += writePredicat("chemin", 62, 2, 63, 1, 0, 1)
  result += writePredicat("chemin", 62, 2, 63, 2, 0, 3)
  result += writePredicat("voisin", 63, 1, 63, 2, 1, 2)
  result += writePredicat("voisin", 63, 1, 63, 2, 1, 3)
  result += writePredicat("chemin", 63, 1, 64, 1, 1, 1)
  result += writePredicat("chemin", 63, 1, 64, 2, 1, 3)
  result += writePredicat("chemin", 63, 2, 64, 1, 2, 1)
  result += writePredicat("chemin", 63, 2, 64, 2, 2, 3)
  result += writePredicat("voisin", 64, 1, 64, 2, 1, 2)
  result += writePredicat("voisin", 64, 1, 64, 2, 1, 3)
  result += writePredicat("chemin", 64, 1, 65, 1, 1, 0)
  result += writePredicat("chemin", 64, 1, 65, 2, 1, 0)

  result += writePredicat("voisin", 72, 1, 72, 2)

  result += writePredicat("chemin", 75, 1, 76, 1)
  result += writePredicat("chemin", 75, 1, 76, 2)

  result += writePredicat("voisin", 83, 1, 83, 2)
  result += writePredicat("chemin", 83, 1, 84, 1)
  result += writePredicat("chemin", 83, 1, 84, 3)

  result += writePredicat("chemin", 88, 3, 89, 3, 0, 3)
  result += writePredicat("chemin", 89, 1, 90, 1, 1, 1)
  result += writePredicat("chemin", 89, 3, 89, 3, 3, 2)
  result += writePredicat("chemin", 89, 3, 90, 3, 2, 3)
  result += writePredicat("chemin", 89, 1, 90, 1, 1, 1)
  result += writePredicat("chemin", 90, 3, 90, 3, 3, 2)
  result += writePredicat("chemin", 90, 3, 91, 3, 2, 0)
  result += writePredicat("chemin", 90, 1, 91, 1, 1, 0)

  result += writePredicat("chemin", 94, 1, 95, 1)
  result += writePredicat("chemin", 94, 1, 95, 2)
  result += writePredicat("chemin", 94, 2, 95, 2)
  result += writePredicat("chemin", 94, 2, 95, 3)
  result += writePredicat("voisin", 95, 1, 95, 2)
  result += writePredicat("voisin", 95, 2, 95, 3)

  return result
}
// voisin(X1, Y1, X2, Y2) :- voisin(X2, Y2, X1, Y1).
// voisin(X1, Y1, X2, Y2) :- chemin(X2, Y2, X1, Y1).

function writePredicat(name, x1, y1, x2, y2, letter1 = 0, letter2 = 0) {
  return `${name}(${x1 * 10 + letter1}, ${y1}, ${x2 * 10 + letter2}, ${y2}).\n`
}

console.log(generator())
