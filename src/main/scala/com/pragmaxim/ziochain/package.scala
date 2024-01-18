package com.pragmaxim

package object ziochain {
  type Nonce = Long
  type Bytes = Array[Byte]
  type HashNumber = BigInt

  def bigEndianByteArray(value: Int): Array[Byte] =
    Array[Byte]((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  def bigEndianByteArray(value: Long): Array[Byte] = {
    var l = value
    val result = new Array[Byte](8)
    for (i <- 7 to 0 by -1) {
      result(i) = (l & 0xffL).toByte
      l >>= 8
    }
    result
  }

}
