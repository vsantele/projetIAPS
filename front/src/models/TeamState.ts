import { PlayerCoord } from "./PlayerPosition";

export default interface TeamState {
  id: string,
  name: string,
  cards: number[],
  playersPositions: PlayerCoord[],
}