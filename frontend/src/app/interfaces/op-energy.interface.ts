
export interface TimeStrike {
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
  elapsedTime?: number;
};

export interface SlowFastGuess {
  guess : "slow" | "fast";
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
  userName: string;
  userId: number;
}

export interface SlowFastGuessOutcome {
  outcome: "slow" | "fast";
  blockHeight: number;
  nLockTime: number;
}

