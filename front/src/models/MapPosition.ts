export default interface MapPosition {
  // Front-end
  mapXRatio: number,
  mapYRatio: number,

  // Back-end
  playerXPosition: number, // number of map column
  playerZPosition: number, // number map row
}
