export interface NbdrStatistics {
  nbdr: {
    avg: number;
    stddev: number;
  }
}

export interface NbdrStatisticsError {
  error: string;
  status: number;
}
