export default interface WinnerResult {
  globalRanking: number[][];
  rankingPoints: number[][];
}

export interface Winner {
  winnerTeam: string;
  winnerPlayer: number;
  ranking: number[][];
}