package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.State.TxsAppliedToState

import scala.collection.immutable.SortedSet

/** Blockchain verifies/validates transaction by trying to apply them to UtxoState,
 * returns valid txs to the Miner which mines a blocks and asks for applying it to the blockchain */
trait State {
  /** @return hash of the latest block */
  def bestBlockHash: Hash

  /** @return current height of blockchain, index of the latest block */
  def height: Long

  /** @return true if any branch has formed in blockchain and was not garbage collected yet */
  def isForked: Boolean

  /** @return latest block or blocks in case chain is currently having equally high competing branches */
  def latestBlocks: List[Block]

  /** Appends block to the tip of blockchain
   *
   * @param verifiedBlock to append
   * @return new amended BlockchainLike instance */
  def append(verifiedBlock: Block): State

  /** @return printable state of UtxoState */
  def printableState: String

  /** @return true if block is orphaned, meaning it does not have a parent block in blockchain */
  def isBlockOrphaned(block: BlockTemplate): Boolean

  /**
   * Validates and applies transactions to the UtxoState
   *
   * @param txs        to validate and apply
   * @return Applied transactions (valid/invalid) and amended copy of BlockchainLike instance
   */
  def applyTxsToUtxoState(txs: SortedSet[Transaction]): (TxsAppliedToState, State)

  /**
   * Find block(s) by index
   *
   * @param index height
   * @return block or blocks in case chain is currently having equally high competing branches
   */
  def findByIndex(index: Long): List[Block]

  /**
   * Find block by its hash
   *
   * @param hash of the block we want to look up
   * @return Maybe a block if it exists in blockchain
   */
  def findByHash(hash: Hash): Option[Block]

  /**
   * Comparing 2 blockchains from the genesis block up until common ancestor block is found
   *
   * @param that blockchain to compare with
   * @return Maybe a block that is a common ancestor to blockchains being compared
   */
  def commonAncestor(that: State): Option[Block]

  /**
   * When a shorter branch is too old, it cannot grow anymore
   * as competing branch has won, such shorter branches need to be removed
   *
   * @return Maybe a block that is a common ancestor to blockchains being compared
   */
  def garbageCollectOrphanedBranches: State

  /** We keep UtxoState instance for each block due to forking, but we only need to keep latest
   * instances when competition of branches is still possible, the rest can be garbage collected
   *
   * @return hashes of blocks that UtxoState version was garbage collected for + amended blockchain instance
   */
  def garbageCollectUtxoState: (IndexedSeq[Hash], State)
}

object State {
  case class TxsAppliedToState(valid: SortedSet[Transaction], invalid: SortedSet[Transaction], height: Long, parentHash: Hash)
}
