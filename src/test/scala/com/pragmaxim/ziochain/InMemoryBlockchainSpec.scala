package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.State.TxsAppliedToState
import zio.Ref
import zio.test.Assertion.{equalTo, fails, hasMessage}
import zio.test.{ZIOSpecDefault, assertTrue, assertZIO}

import scala.collection.immutable.TreeSet

object InMemoryBlockchainSpec extends ZIOSpecDefault {

  private def forkBlockchainWith(parentHash: Hash, atHeight: Long, bc: State, txs: Transactions) = {
    (atHeight to bc.height+4).foldLeft(parentHash -> bc) { case ((newParentHash, newBc), childIdx) =>
      val newBlock = Miner.mineNextBlock(childIdx, newParentHash, txs, Miner.StdMiningTargetNumber)
      newBlock.hash -> newBc.append(newBlock)
    }
  }

  private def generateNewBlockchain(txs: Transactions, height: Long = 3) = {
    (1L to height).foldLeft(Miner.verifiedGenesisBlock.hash -> InMemoryState.fromGenesis) { case ((parentHash, bc), idx) =>
      val newBlock = Miner.mineNextBlock(idx, parentHash, txs, Miner.StdMiningTargetNumber)
      newBlock.hash -> bc.append(newBlock)
    }
  }

  def spec =
    suite("blockchain")(
      test("provide correct height") {
        val bcWithGenesisOnly = InMemoryState.fromGenesis
        val newBlock = Miner.mineNextBlock(1, Miner.verifiedGenesisBlock.hash, Transactions(TreeSet(Transaction(100, "Alice", "Bob"))), Miner.StdMiningTargetNumber)
        assertTrue(
          bcWithGenesisOnly.height == 0,
          bcWithGenesisOnly.append(newBlock).height == 1
        )
      },
      test("return correct heads") {
        val bcWithGenesisOnly = InMemoryState.fromGenesis
        val newBlock = Miner.mineNextBlock(1, Miner.verifiedGenesisBlock.hash, Transactions(TreeSet(Transaction(100, "Alice", "Bob"))), Miner.StdMiningTargetNumber)
        val evolvedBc = bcWithGenesisOnly.append(newBlock)
        assertTrue(
          bcWithGenesisOnly.height == 0,
          evolvedBc.height == 1,
          evolvedBc.latestBlocks == List(newBlock)
        )
      },
      test("append valid blocks and reject invalid") {
        val bcWithGenesisOnly = InMemoryState.fromGenesis
        val validBlock = Miner.mineNextBlock(1, Miner.verifiedGenesisBlock.hash, Transactions(TreeSet(Transaction(100, "Alice", "Bob"))), Miner.StdMiningTargetNumber)
        assertTrue(bcWithGenesisOnly.append(validBlock) != null)
      },
      test("reject invalid blocks") {
        val bcWithGenesisOnly = InMemoryState.fromGenesis
        val invalidIndexBlock = Miner.mineNextBlock(256, Miner.verifiedGenesisBlock.hash, Transactions(TreeSet(Transaction(100, "Alice", "Bob"))), Miner.StdMiningTargetNumber)
        val orphanBlock = Miner.mineNextBlock(1, Hash.sha256("Alice".getBytes("UTF-8")), Transactions(TreeSet(Transaction(100, "Alice", "Bob"))), Miner.StdMiningTargetNumber)
        val invalidTxsBlock = Miner.mineNextBlock(1, Miner.verifiedGenesisBlock.hash, Transactions(TreeSet.empty), Miner.StdMiningTargetNumber)

        for {
          state <- Ref.make(bcWithGenesisOnly)
          mempool <- Ref.make(MemPool.empty)
          chain = BlockChain(state, mempool)
          invalidIndexBlockExit = chain.applyNewBlock(invalidIndexBlock).exit
          orphanBlockExit = chain.applyNewBlock(orphanBlock).exit
          invalidTxsBlockExit = chain.applyNewBlock(invalidTxsBlock).exit
          _ <- assertZIO(invalidIndexBlockExit)(fails(hasMessage(equalTo("Password store is not initialized")))) &&
                assertZIO(orphanBlockExit)(fails(hasMessage(equalTo("Password store is not initialized")))) &&
                assertZIO(invalidTxsBlockExit)(fails(hasMessage(equalTo("Password store is not initialized"))))
        } yield assertTrue(true)
      },
      test("find blocks by index") {
        assertTrue(
          InMemoryState.fromGenesis.findByIndex(1).isEmpty,
            InMemoryState.fromGenesis.findByIndex(0).nonEmpty
        )
      },
      test("find blocks by hash") {
        assertTrue(
          InMemoryState.fromGenesis.findByHash(Miner.Zero_Hash).isEmpty,
            InMemoryState.fromGenesis.findByHash(Miner.verifiedGenesisBlock.hash).nonEmpty
        )
      },
      test("find blocks by hash") {
        val dummyTxs_1 = Transactions(TreeSet(Transaction(100, "Alice", "Bob")))
        val dummyTxs_2 = Transactions(TreeSet(Transaction(100, "Bob", "Tom")))

        val (commonHeadHash_1, commonBlockChain_1) = generateNewBlockchain(dummyTxs_1)
        val (commonHeadHash_2, commonBlockChain_2) = generateNewBlockchain(dummyTxs_1)

        val (differentHeadHash_1, differentBlockChain_1) = forkBlockchainWith(commonHeadHash_1, commonBlockChain_1.height+1, commonBlockChain_1, dummyTxs_1)
        val (differentHeadHash_2, differentBlockChain_2) = forkBlockchainWith(commonHeadHash_2, commonBlockChain_2.height+1, commonBlockChain_2, dummyTxs_2)

        val longerBlockChain_2 =
          differentBlockChain_2.append(
            Miner.mineNextBlock(
              differentBlockChain_2.height+1,
              differentHeadHash_2,
              dummyTxs_2,
              Miner.StdMiningTargetNumber
            )
          )

        assertTrue(
          // check heads of blockchains are the same
          commonHeadHash_1 == commonHeadHash_2,
          commonBlockChain_1.findByHash(commonHeadHash_2).nonEmpty,
          commonBlockChain_2.findByHash(commonHeadHash_1).nonEmpty,
          // different blockchains should have different heads
          differentHeadHash_1 != differentHeadHash_2,

          // check equally high blockchain for common ancestor
          differentBlockChain_1.commonAncestor(differentBlockChain_2).map(_.hash).contains(commonHeadHash_1),
          differentBlockChain_2.commonAncestor(differentBlockChain_1).map(_.hash).contains(commonHeadHash_1),
          // check longer blockchain with a shorter one for common ancestor
          differentBlockChain_1.commonAncestor(longerBlockChain_2).map(_.hash).contains(commonHeadHash_1),
          longerBlockChain_2.commonAncestor(differentBlockChain_1).map(_.hash).contains(commonHeadHash_1),
          // check common ancestor of evolved blockchain
          differentBlockChain_2.commonAncestor(longerBlockChain_2).map(_.hash).contains(differentHeadHash_2)
        )
      },
      test("garbage collect orphaned branches") {
        val dummyTxs_1 = Transactions(TreeSet(Transaction(100, "Alice", "Bob")))
        val dummyTxs_2 = Transactions(TreeSet(Transaction(100, "Bob", "Tom")))
        val dummyTxs_3 = Transactions(TreeSet(Transaction(100, "Tom", "Jane")))

        // build a forked blockchain
        val (commonHeadHash, commonBlockChain) = generateNewBlockchain(dummyTxs_1)
        val (_, evolvedBlockChain) = forkBlockchainWith(commonHeadHash, commonBlockChain.height+1, commonBlockChain, dummyTxs_2)
        val (forkedHeadHash, forkedBlockChain) = forkBlockchainWith(commonHeadHash, commonBlockChain.height+1, evolvedBlockChain, dummyTxs_3)

        val winningBlockChain = forkedBlockChain.append(Miner.mineNextBlock(forkedBlockChain.height+1, forkedHeadHash, dummyTxs_3, Miner.StdMiningTargetNumber))
        val forkFreeBlockChain = winningBlockChain.garbageCollectOrphanedBranches

        assertTrue(
          // cleanup does nothing as no branch has won yet, they are equally high
          forkedBlockChain.garbageCollectOrphanedBranches == forkedBlockChain,
          // cleanup now removes the less evolved branch from the blockchain
          forkFreeBlockChain != winningBlockChain,
          // check that there are now forks
          !forkFreeBlockChain.isForked,
        )
      },
      test("apply transactions into utxo state") {
        val validTxs =
          TreeSet(
            Transaction(100, "Bob", "Alice", System.currentTimeMillis()),
            Transaction(100, "Alice", "Bob", System.currentTimeMillis()+1),
            Transaction(50, "Bob", "Alice", System.currentTimeMillis()+2),
            Transaction(50L, "Alice", "Tom", System.currentTimeMillis()+3)
          )
        val invalidTxs =
          TreeSet(
            Transaction(50L, "Alice", "Tom"),
            Transaction(50L, "Unknown", "Tom")
          )
        val (TxsAppliedToState(valid, invalid, _, _), newBlockChain) = InMemoryState.fromGenesis.applyTxsToUtxoState(validTxs ++ invalidTxs)
        assertTrue(
          newBlockChain.asInstanceOf[InMemoryState].utxoStateByHash == Map(Miner.verifiedGenesisBlock.hash -> Map("Bob" -> 99999950L, "Alice" -> 0L, "Tom" -> 50L)),
            valid == validTxs,
            invalid == invalidTxs
        )
      },
      test("garbage collect utxo state") {
        val dummyTxs_1 = Transactions(TreeSet(Transaction(100, "Alice", "Bob")))
        val dummyTxs_2 = Transactions(TreeSet(Transaction(100, "Bob", "Tom")))
        val dummyTxs_3 = Transactions(TreeSet(Transaction(100, "Tom", "Jane")))

        // build a forked blockchain
        val (commonHeadHash, commonBlockChain) = generateNewBlockchain(dummyTxs_1, 20)
        val (evolvedHeadHash, evolvedBlockChain) = forkBlockchainWith(commonHeadHash, commonBlockChain.height+1, commonBlockChain, dummyTxs_2)
        val (forkedHeadHash, forkedBlockChain) = forkBlockchainWith(commonHeadHash, commonBlockChain.height+1, evolvedBlockChain, dummyTxs_3)


        // garbage collect old versions of UtxoState
        val garbageCollectedHashes = forkedBlockChain.garbageCollectUtxoState._1
        assertTrue(
          // short chain should not be garbage collected
          generateNewBlockchain(dummyTxs_1)._2.garbageCollectUtxoState._1.isEmpty,
          garbageCollectedHashes.size == 10,
          !garbageCollectedHashes.contains(commonHeadHash),
          !garbageCollectedHashes.contains(evolvedHeadHash),
          !garbageCollectedHashes.contains(forkedHeadHash)
        )
      }
    )

}
