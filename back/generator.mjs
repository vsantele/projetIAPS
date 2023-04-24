import fs from "fs/promises"
// d = droit, v = virage
// Si d: ['d', 8,3] -> 8 cases de long, 3 positions possible
// Si d: ['d', 8,3,2] -> 8 cases de long, 3 positions possible, arbre position 2.
// si v: ['v',2, 1,2] -> 2 cases de larges, 1 case position 1, 2 cases en positions 2
// si v: ['v',3, 1,0, 2] -> 2 cases de larges, 1 case position 1,arbre position 2, 2 cases en positions 2
const board = [
  ["d", 8, 3, 0],
  ["v", 2, 1, 2],
  ["v", 2, 1, 2],
  ["d", 8, 2, 0],
  ["d", 4, 3, 0],
  ["d", 3, 4, 3],
  ["v", 4, 1, 1, 0, 2],
  ["v", 4, 1, 1, 0, 2],
  ["d", 8, 4, 3],
  ["d", 27, 2, 0],
  ["v", 2, 1, 2],
  ["v", 2, 1, 2],
  ["d", 8, 2, 0],
  ["d", 3, 1, 0],
  ["d", 8, 2, 0],
  ["d", 5, 3, 2],
  ["v", 3, 1, 0, 2],
  ["v", 3, 1, 0, 2],
  ["d", 4, 3, 2],
  ["d", 1, 3, 0],
]

function generator() {
  let x = 1
  let result = ""
  for (let iBoard = 0; iBoard < board.length; iBoard++) {
    const [type, ...args] = board[iBoard]
    if (type === "d") {
      const [long, larg, tree] = args
      for (let i = 0; i < long; i++) {
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
            result += writePredicat("voisin", x, y, x, y - 1)
          }
        }
        x++
      }
      if (iBoard < board.length - 1) {
        const [type, ...args] = board[iBoard + 1]
        if (type === "d") {
          const [long, larg, tree] = args
          for (let y = 1; y <= larg; y++) {
            if (tree === y) {
              continue
            }
          }
        }
      }
    } else if (type === "v") {

  }
  return result
}
// voisin(X1, Y1, X2, Y2) :- voisin(X2, Y2, X1, Y1).
// voisin(X1, Y1, X2, Y2) :- chemin(X2, Y2, X1, Y1).

function writePredicat(name, x1, y1, x2, y2) {
  return `${name}(${x1}, ${y1}, ${x2}, ${y2}).\n`
}

console.log(generator())
