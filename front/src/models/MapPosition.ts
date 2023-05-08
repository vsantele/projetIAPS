export default interface MapPosition {
  // Front-end
  mapXRatio: number,
  mapYRatio: number,

  // Back-end
  playerForward: number, // number of map column
  playerLateral: number, // number map row
}
