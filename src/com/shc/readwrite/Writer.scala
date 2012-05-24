package com.shc.readwrite

import java.io.{ File, FileOutputStream, BufferedOutputStream }

class IterableWriter(path: String, mode: Boolean) {
  protected val f = new File(path)
  var open: Boolean = (this.f.exists && this.f.canWrite) || this.f.createNewFile
  protected var writer: BufferedOutputStream = _
  if (this.open) {
    writer = new BufferedOutputStream(new FileOutputStream(f, mode))
  }
  protected val onStack = new scala.collection.mutable.ArrayStack[Byte]()
  protected val record = new scala.collection.mutable.ArrayStack[Int]()

  def write(str: String, charset: String = "UTF-8") {
    if (this.open) this.write(str.getBytes(charset))
  }

  def write(int: Int) {
    if (this.open) this.write(int.toString)
  }

  def write(char: Char) {
    if (this.open) this.write(char.toByte)
  }

  def write(byte: Byte) {
    if (this.open) {
      this.record.push(1)
      this.onStack.push(byte)
    }
  }

  def write(bytes: Array[Byte]) {
    if (this.open) {
      this.record.push(bytes.length)
      bytes.foreach(b => this.onStack.push(b))
    }
  }

  def clear {
    this.record.clear
    this.onStack.clear
  }

  // ArrayStack's reverse method including bug in version 2.9.0.1
  def byteHistory = this.record.toArray

  def writtenSize = this.onStack.length

  def revert(back: Int) = {
    if (back < 0 || back > this.byteHistory.length) Array[Byte]()
    else {
      val temp = this.byteHistory
      val resultSize = temp(back - 1)
      val historySum = temp.slice(0, back).foldLeft(0)((i, j) => i + j)
      this.onStack.toArray.slice(historySum - resultSize, historySum).reverse
    }
  }

  def undo = {
    if (this.record.length > 0 && this.open) {
      val undoSize = this.record.pop
      val removed = new Array[Byte](undoSize)
      Range(0, undoSize).foreach(i => removed(undoSize - 1 - i) = this.onStack.pop)
      removed
    } else Array[Byte]()
  }

  def save: Boolean = {
    if (this.open) {
      val result =
        try {
          this.writer.write(this.onStack.toArray.reverse)
          true
        } catch {
          case e: Exception => false
        }
      this.onStack.clear
      this.record.clear
      result
    } else false
  }

  def close {
    if (this.open) {
      this.open = false
      this.onStack.clear
      this.record.clear
      this.writer.close
      this.writer = null
    }
  }

  def chabudaiFlipping {
    if (this.open) {
      this.open = false
      this.onStack.clear
      this.record.clear
      this.writer.close
      this.writer = null
      this.f.delete
    }
  }
}
