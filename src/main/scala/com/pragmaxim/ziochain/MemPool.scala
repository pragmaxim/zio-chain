package com.pragmaxim.ziochain

import scala.collection.immutable.{SortedSet, TreeSet}

case class MemPool(txs: SortedSet[Transaction]) {
  def add(newTxs: IndexedSeq[Transaction]): MemPool =
    copy(txs = this.txs ++ newTxs)
  def remove(oldTxs: Set[Transaction]): MemPool =
    copy(txs = this.txs -- oldTxs)
}

object MemPool {
  def empty: MemPool = MemPool(TreeSet.empty)
}