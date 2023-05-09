import TeamState from "./TeamState";

export default interface JsState {
  currentCountry: string;
  cards: number[];
  teams: TeamState[];
}