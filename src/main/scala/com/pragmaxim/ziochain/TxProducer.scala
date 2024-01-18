package com.pragmaxim.ziochain

import zio.stream.ZStream
import zio.{Ref, Schedule, UIO, durationInt}

import scala.collection.immutable.ArraySeq

object TxProducer {

  /** Peer receives txs from other peers for demonstration purposes, it does not transmit txs to them */
  def peerTxStream(memPool: Ref[MemPool]): UIO[Unit] =
    ZStream
      .repeatWithSchedule((), Schedule.spaced(1.second))
      .foreach(_ => memPool.update(_.add(ArraySeq(Transaction(100, "Alice", "Bob"), Transaction(15, "Bob", "Tom")))))

  /** Wallet only simulates transaction generation by a user */
  def walletTxStream(memPool: Ref[MemPool]): UIO[Unit] =
    ZStream
      .repeatWithSchedule((), Schedule.spaced(500.millis))
      .foreach(_ => memPool.update(_.add(ArraySeq(Transaction(100, "Bob", "Alice")))))

}
