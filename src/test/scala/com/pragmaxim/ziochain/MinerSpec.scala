package com.pragmaxim.ziochain

import com.pragmaxim.ziochain.Miner._
import zio.ZIO
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

import scala.collection.immutable.TreeSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MinerSpec extends ZIOSpecDefault {

  def spec =
    suite("miner")(
      test("should use hash with correct equality and hash") {
        assertTrue(
          Miner.Zero_Hash == Miner.Zero_Hash,
          Miner.Zero_Hash.toHexString == Miner.Zero_Hash.bytes.map("%02X" format _).mkString("0x", "", "")
        )
      },
      test("should use block with correct equality") {
        assertTrue(
          Miner.verifiedGenesisBlock.template == Miner.verifiedGenesisBlock.template
        )
      },
      test("should generate mining target number") {
        assertTrue(
          Miner.targetByLeadingZeros(31) == 255
        )
      },
      test("should generate mining target number should fail") {
        val exit = ZIO.attempt(Miner.targetByLeadingZeros(32)).exit
        assertZIO(exit)(fails(hasMessage(equalTo("requirement failed"))))
      },
      test("should mine block") {
        val genesisBlock = Miner.verifiedGenesisBlock
        val genesisHash = BlockTemplate.cryptoHash(0, Zero_Hash, Transactions(TreeSet(genesisTx)), StdMiningTargetNumber, genesisBlock.template.nonce)
        genesisBlock.template.verifyThisHasBeenMinedProperly()

        val veryDifficultTargetNumber = targetByLeadingZeros(31)
        assertTrue(genesisBlock.hash == genesisHash)
        val exit =
          ZIO.attempt (
            Await.result(
              Future(Miner.mineNextBlock(1, genesisHash, Transactions(TreeSet(genesisTx)), veryDifficultTargetNumber)),
              500.millis
            )
          ).exit
        assertZIO(exit)(fails(hasMessage(equalTo("Future timed out after [500 milliseconds]"))))
      },
    )
}
