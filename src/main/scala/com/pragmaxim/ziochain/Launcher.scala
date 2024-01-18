package com.pragmaxim.ziochain

import zio.stream.ZStream
import zio.{Ref, Schedule, Task, ZIOAppDefault, durationInt}

object Launcher extends ZIOAppDefault {

  private def stream(blockchain: BlockChain) : Task[Unit] =
    ZStream.repeatWithSchedule((), Schedule.spaced(3.second))
      .mapZIO(_ => blockchain.getMemPoolTxs)    // collect TXs from mempool
      .filter(_.nonEmpty)                       // drop empty set
      .mapZIO(blockchain.applyTxsToUtxoState)   // apply them to UtxoState
      .filter(_.valid.nonEmpty)                 // drop empty set
      .mapZIO(Miner.mineNextValidBlock)         // Mine new block from valid TXs
      .tap(blockchain.applyNewBlock)            // apply it to blockchain
      .foreach(blockchain.removeTxsFromMemPool) // remove TXs from mempool

  def run =
    for {
      memPoolRef    <- Ref.make(MemPool.empty)
      blockChainRef <- Ref.make(InMemoryState.fromGenesis)
      peer          <- TxProducer.peerTxStream(memPoolRef).fork
      wallet        <- TxProducer.walletTxStream(memPoolRef).fork
      miner         <- stream(BlockChain(blockChainRef, memPoolRef)).fork
      _             <- peer.zip(wallet).zip(miner).join
    } yield ()
}
