package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.State.TxsAppliedToState
import zio.{Ref, Task, UIO, ZIO}

import scala.collection.immutable.SortedSet

case class BlockChain(state: Ref[State], memPool: Ref[MemPool]) {

  def getMemPoolTxs: UIO[SortedSet[Transaction]] =
    memPool.get.map(_.txs)

  def applyTxsToUtxoState(txs: SortedSet[Transaction]): UIO[TxsAppliedToState] =
    state.modify(_.applyTxsToUtxoState(txs))

  def removeTxsFromMemPool(newBlock: Block): UIO[Unit] =
    memPool.update(_.remove(newBlock.template.transactions.txs))

  def applyNewBlock(newBlock: Block): Task[State] =
    for {
      state2 <- state.get
      block = newBlock.template
      blockHash = newBlock.hash
      blockHeight = newBlock.template.index
      newState <-  if (state2.isBlockOrphaned(block)) {
        ZIO.fail(new IllegalArgumentException(s"Orphan block $blockHash does not have a parent ${block.parentHash} at height $blockHeight"))
      } else if (block.transactions.txs.isEmpty) {
        ZIO.fail(new IllegalArgumentException(s"Block $blockHash has no transactions at height $blockHeight"))
      } else {
        state.updateAndGet(_.append(newBlock))
      }
      _ <- ZIO.log(s"New UtxoState : ${state2.printableState}")
    } yield newState

}
