import * as BlueBird from "bluebird";
import bitcoinApi from "../../api/bitcoin/bitcoin-api-factory";

export class OpStatisticService {
  constructor() {}

  async calculateStatistics(blockHeight: number, blockSpan: number) {
    const nbdrStatisticsList: number[] = [];
    try {
      await BlueBird.map(
        Array.from(Array(100).keys()),
        async (i) => {
          const startBlockHeight = blockHeight - i * blockSpan;
          const endBlockHeight = startBlockHeight - blockSpan + 1;
          try {
            const startblockHash = await bitcoinApi.$getBlockHash(
              startBlockHeight - blockSpan - i
            );
            const startBlock = await bitcoinApi.$getBlock(startblockHash);
            const endblockHash = await bitcoinApi.$getBlockHash(
              endBlockHeight - 1 - blockSpan - i
            );
            const endBlock = await bitcoinApi.$getBlock(endblockHash);
            const nbdr =
              (blockSpan * 600 * 100) /
              (startBlock.timestamp - endBlock.timestamp);
            console.log(
              `start block: ${startBlockHeight}, end block: ${endBlockHeight}, span: ${blockSpan}, nbdr: ${nbdr}`
            );
            nbdrStatisticsList.push(nbdr);
          } catch (error) {
            console.log(
              "Error while calculating nbdr",
              startBlockHeight,
              endBlockHeight,
              error
            );
          }
        },
        {
          concurrency: 20,
        }
      );
      const length = nbdrStatisticsList.length;
      const mean = nbdrStatisticsList.reduce((a, b) => a + b) / length;
      return {
        avg: mean,
        stddev: Math.sqrt(
          nbdrStatisticsList.map((x) => Math.pow(x - mean, 2)).reduce((a, b) => a + b) /
            length -
            1
        ),
      };
    } catch (error) {
      console.log("Error while calculating nbdr", error);
      return {
        error: "Something went wrong",
        status: 500,
      };
    }
  }
}

export default new OpStatisticService();
