
export interface TimeStrike {
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
};

export interface TimeStrikesHistory {
  owner: string;
  blockHeight: number;
  nLockTime: number;
  mediantime: number;
  creationTime: number;
  archiveTime: number;
  wrongResults: number;
  rightResults: number;
};


export interface TimeStrikeId {
  value: number;
};

export interface TimeStrikeDB {
  id: TimeStrikeId;
  value: TimeStrike;
};

export interface SlowFastGuess {
  guess : "slow" | "fast";
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
  userName: string;
  userId: number;
}

export interface SlowFastResult {
  guess : "slow" | "fast";
  result: "wrong" | "right";
  blockHeight: number;
  nLockTime: number;
  creationTime: number;
}

export interface AccountToken {
  accountToken: string;
}

export interface AccountSecret {
  value: string;
}

export interface UserId {
  userId: number;
  userName: string;
}

export interface AlphaNumString {
  value: string;
}

// positive number
export interface BlockHeight {
  value: number;
}

// natural number TODO: maybe positive?
export interface NLockTime {
  value: number;
}

export interface SlowFastGuessValue {
  value: number;
}

export interface BlockHash {
  value: string;
}

export interface BlockHeader {
  height: number;
  version: number;
  currentBlockHash: string;
  previousBlockHash: string;
  merkleRoot: string;
  timestamp: number;
  difficulty: number;
  nonce: number;
  reward: number;
  chainWork: string;
  medianTime: number;
}

export interface ConfirmedBlockHeight {
  value: number
}

export interface BlockSpan {
  startBlockHeight: number, 
  endBlockHeight: number
}
