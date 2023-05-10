import { PlayerPosition } from "./PlayerPosition";

export default interface PrologState {
  country: string;
  cards: number[];
  countriesCards: number[][];
  playersPositions: PlayerPosition;
  selectedCard: number;
}