package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.State.TxsAppliedToState
import zio.{Task, ZIO}

import scala.collection.immutable.TreeSet

/** Miner is pulling txs from mempool and applies them to UtxoState, valid/verified txs are then used to mine a block
 * which is passed to blockchain */
object Miner {

  final val genesisTx = Transaction(100000000, "Alice", "Bob")

  /** In real blockchain, it is adjusted based on various properties like network load, mining power, price, etc. */
  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  /** Hash of non-existent block to be used as a parent for genesis block */
  val Zero_Hash: Hash =
    new Hash(
      Hash.newSha256Instance.digest(
        "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks".getBytes("UTF-8")
      )
    )

  final val verifiedGenesisBlock = Miner.mineNextBlock(
    index = 0, // The very first block
    parentHash = Zero_Hash, // Let's assume this is by definition for the Genesis block.
    transactions = Transactions(TreeSet(genesisTx)),
    StdMiningTargetNumber,
  )

  /** Method for building mining target number */
  def targetByLeadingZeros(zeros: Int): HashNumber = {
    require(zeros < Hash.Sha256NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        }
        else {
          0xff.toByte
        }
      }

    BigInt(1, bytes)
  }

  def mineNextValidBlock(txs: TxsAppliedToState): Task[Block] = {
    val block = Miner.mineNextBlock(txs.height + 1, txs.parentHash, Transactions(txs.valid), Miner.StdMiningTargetNumber)
    if (block.hash.toNumber >= Miner.StdMiningTargetNumber)
      ZIO.fail(new IllegalStateException("Unable to find solution with Nonce<Long.MinValue, Long.MaxValue>"))
    else
      ZIO.log(s"Mined new block of ${txs.valid.size} txs : ${block.hash}").as(block)
  }

  /** Hash BlockTemplate with an increasing nonce until we get a hash number that is lower than mining target number */
  def mineNextBlock(
                     index: Long,
                     parentHash: Hash,
                     transactions: Transactions,
                     miningTargetNumber: BigInt,
                   ): Block = {
    var currentNonce: Nonce = -1
    var currentSolution: Hash = null
    do {
      currentNonce += 1
      currentSolution = BlockTemplate.cryptoHash(index, parentHash, transactions, miningTargetNumber, currentNonce)
    } while (currentSolution.toNumber >= miningTargetNumber && currentNonce < Long.MaxValue)
    Block(currentSolution, BlockTemplate(index, parentHash, transactions, miningTargetNumber, currentNonce))
  }
}
