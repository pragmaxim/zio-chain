package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.InMemoryState._
import com.pragmaxim.ziochain.State.TxsAppliedToState

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet, TreeMap, TreeSet}
import scala.util.{Failure, Success, Try}

/**
 * Non-persistent Blockchain which holds all structures in memory
 *
 * @param lastGarbageCollectionHeight checkpoint of the last garbage collection execution
 * @param indexCache                  cache of blocks by height, each height can have multiple blocks due to forking
 * @param hashCache                   cache of blocks by their hash
 * @param utxoStateByHash             UtxoState corresponding to blocks, we need to have multiple instances due to forking
 */
case class InMemoryState(
                               lastGarbageCollectionHeight: Long,
                               indexCache: SortedMap[Long, List[Block]],
                               hashCache: Map[Hash, BlockTemplate],
                               utxoStateByHash: Map[Hash, UtxoState]) extends State {
  def bestBlockHash: Hash =
    latestBlocks.maxBy(_.template.transactions.txs.size).hash

  def height: Long = indexCache.last._1

  def isForked: Boolean = indexCache.size == hashCache.size && indexCache.forall(_._2.tails.isEmpty)

  def latestBlocks: List[Block] = findByIndex(height)

  def isBlockOrphaned(block: BlockTemplate): Boolean = !indexCache.get(block.index - 1).exists(_.exists(_.hash == block.parentHash))

  def append(verifiedBlock: Block): InMemoryState = {
    val block = verifiedBlock.template
    val blockHash = verifiedBlock.hash
    val newBlockChain = InMemoryState(
      lastGarbageCollectionHeight,
      indexCache.adjust(block.index)(_.fold(verifiedBlock :: Nil)(verifiedBlock :: _)),
      hashCache.updated(blockHash, block),
      utxoStateByHash.updated(blockHash, utxoStateByHash(block.parentHash))
    )
    val highEnoughForGarbageCollection =
      newBlockChain.height - lastGarbageCollectionHeight > Settings.OrphanedForksGarbageCollectionLength
    if (highEnoughForGarbageCollection) {
      newBlockChain.garbageCollectOrphanedBranches.garbageCollectUtxoState._2
    } else
      newBlockChain
  }

  def printableState: String = {
    val parentHash = bestBlockHash
    val utxoStateForHash = utxoStateByHash(parentHash)
    utxoStateForHash.map { case (address, value) => s"$address : $value" }.mkString(", ")
  }

  def applyTxsToUtxoState(txs: SortedSet[Transaction]): (TxsAppliedToState, InMemoryState) = {
    def transfer(value: Long, input: String, output: String, utxoState: Map[String, Long]): Try[Map[String, Long]] = {
      utxoState.get(input).map {
        case existingInputValue if existingInputValue >= value =>
          Success(utxoState.updated(input, existingInputValue - value).adjust(output)(_.fold(value)(_ + value)))
        case existingInputValue =>
          Failure(new IllegalArgumentException(s"Address $input has only $existingInputValue balance to cover request for $value"))
      }.getOrElse(Failure(new IllegalArgumentException(s"Input address $input does not exist")))
    }

    val parentHash = bestBlockHash
    val utxoStateForHash = utxoStateByHash(parentHash)

    val validTxs = TreeSet.newBuilder[Transaction]
    val invalidTxs = TreeSet.newBuilder[Transaction]
    val newUtxoStateForHash = txs.foldLeft(utxoStateForHash) { case (utxoStateAcc, tx) =>
      transfer(tx.value, tx.input, tx.output, utxoStateAcc) match {
        case Success(newUtxoStateAcc) =>
          validTxs.addOne(tx)
          newUtxoStateAcc
        case Failure(_) =>
          invalidTxs.addOne(tx)
          utxoStateAcc
      }
    }
    TxsAppliedToState(validTxs.result(), invalidTxs.result(), height, parentHash) -> copy(utxoStateByHash = utxoStateByHash.updated(parentHash, newUtxoStateForHash))
  }


  def findByIndex(index: Long): List[Block] = indexCache.getOrElse(index, List.empty)

  def findByHash(hash: Hash): Option[Block] = hashCache.get(hash).map(Block(hash, _))

  def commonAncestor(that: State): Option[Block] = {
    var lowestHeight = Math.min(height, that.height)
    var ancestor = Option.empty[Block]
    // performance optimization, to demonstrate real blockchains
    while (ancestor.isEmpty && lowestHeight >= 0) {
      val thatBlocks = that.findByIndex(lowestHeight)
      val thisBlocks = findByIndex(lowestHeight)
      ancestor = thatBlocks.find { thatBlock =>
        thisBlocks.exists(_.hash == thatBlock.hash)
      }
      lowestHeight -= 1
    }
    ancestor
  }

  def garbageCollectOrphanedBranches: InMemoryState = {
    val _ :: tailBlocks = indexCache.last._2

    if (tailBlocks.nonEmpty) { // Ongoing fork detected, waiting until one branch wins ...
      this
    } else {
      @tailrec
      def cleanupBranch(newCleanupHeight: Long, childIndex: Long, parentHash: Hash, newBlockChain: InMemoryState): InMemoryState = {
        val newIndexCache = newBlockChain.indexCache.adjust(childIndex)(_.get.filter(_.hash == parentHash))
        val newHashCache =
          newBlockChain.indexCache(childIndex)
            .filter(_.hash != parentHash)
            .foldLeft(newBlockChain.hashCache) { case (acc, block) => acc - block.hash }
        if (childIndex > lastGarbageCollectionHeight && parentHash != Miner.Zero_Hash) {
          cleanupBranch(
            newCleanupHeight,
            childIndex - 1,
            newIndexCache(childIndex).head.template.parentHash,
            InMemoryState(newCleanupHeight, newIndexCache, newHashCache, utxoStateByHash)
          )
        } else {
          InMemoryState(newCleanupHeight, newIndexCache, newHashCache, utxoStateByHash)
        }
      }

      def findLastSplit: Option[(Long, List[Block])] =
        indexCache
          .takeRight(Settings.OrphanedForksGarbageCollectionLength)
          .toSeq
          .findLast(_._2.tail.isEmpty)

      // Garbage collecting shorter branches
      findLastSplit match {
        case None =>
          this
        case Some((idx, blocks)) =>
          cleanupBranch(idx - 1, idx - 1, blocks.head.template.parentHash, this)
      }
    }
  }

  def garbageCollectUtxoState: (IndexedSeq[Hash], InMemoryState) =
    if (height <= Settings.UtxoStateGarbageCollectionStart) {
      Vector.empty -> this
    } else {
      val (newUtxoState, garbageCollectedHashes) =
        indexCache
          .takeRight(Settings.UtxoStateGarbageCollectionStart)
          .take(Settings.UtxoStateGarbageCollectionStop)
          .flatMap(_._2.map(_.hash))
          .foldLeft(utxoStateByHash, Vector.empty[Hash]) { case ((utxoStateAcc, gcHashes), hashToGarbageCollect) =>
            utxoStateAcc.removed(hashToGarbageCollect) -> gcHashes.appended(hashToGarbageCollect)
          }
      garbageCollectedHashes -> copy(utxoStateByHash = newUtxoState)
    }
}

object InMemoryState {

  /** Balance by address */
  type UtxoState = Map[String, Long]

  implicit class MapPimp[K, V](underlying: Map[K, V]) {
    def adjust(k: K)(f: Option[V] => V): Map[K, V] = underlying.updated(k, f(underlying.get(k)))
  }

  /** Create Blockchain from genesis block */
  def fromGenesis: State = {
    new InMemoryState(
      lastGarbageCollectionHeight = 0,
      TreeMap(0L -> List(Miner.verifiedGenesisBlock)),
      Map(Miner.verifiedGenesisBlock.hash -> Miner.verifiedGenesisBlock.template),
      Map(Miner.verifiedGenesisBlock.hash -> Map(Miner.genesisTx.output -> Miner.genesisTx.value))
    )
  }

  implicit class SortedMapPimp[K, V](underlying: SortedMap[K, V]) {
    def adjust(k: K)(f: Option[V] => V): SortedMap[K, V] = underlying.updated(k, f(underlying.get(k)))
  }
}
