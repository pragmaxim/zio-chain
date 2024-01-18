package com.pragmaxim.ziochain

object Settings {
  /** less developed branches need to be garbage collected */
  val OrphanedForksGarbageCollectionLength = 10

  /** old versions of UtxoState need to be garbage collected : (height - start; height - stop) */
  val UtxoStateGarbageCollectionStart = 20
  val UtxoStateGarbageCollectionStop = 10
}
