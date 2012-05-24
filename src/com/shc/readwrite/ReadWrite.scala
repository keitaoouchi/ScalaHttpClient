package com.shc.readwrite

import java.io.{ File, FileInputStream, BufferedInputStream, InputStream }
import java.io.{ BufferedReader, FileReader }
import java.io.{ FileOutputStream, BufferedOutputStream }
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

object ReadWrite {

  def readString(path: String, bufferSize: Int = 5120):StringReader = {
    new StringReader(path, bufferSize)
  }
  
  def readBytes(path: String, bufferSize: Int = 5120):BytesReader = {
    new BytesReader(path, bufferSize)
  }
  
  def readStream(stream: InputStream, bufferSize: Int = 5120): StreamReader = {
    new StreamReader(stream, bufferSize)
  }

  def writeIter(path: String, mode: Boolean = false):IterableWriter = {
    new IterableWriter(path, mode)
  }

  def read(bis: InputStream): ArrayBuffer[Byte] = {
    val bufferSize = 2048
    val buffer = new Array[Byte](2048)
    val result = new ArrayBuffer[Byte]()
    var startAt = 0
    var readed = bis.read(buffer, startAt, bufferSize)
    while (readed > 0) {
      startAt += readed
      result.appendAll(buffer.slice(0, readed))
      readed = bis.read(buffer, 0, bufferSize)
    }
    result
  }

  def read(path: String): ArrayBuffer[Byte] = {
    val f = new File(path)
    if (f.exists && f.canRead) {
      val fis = new FileInputStream(f)
      val bis = new BufferedInputStream(fis)
      val resultContainer = this.read(bis)
      bis.close
      resultContainer
    } else {
      ArrayBuffer()
    }
  }

  def writeOver(path: String, bytes: Array[Byte], mode: Boolean = false): Boolean = {
    val f = new File(path)
    if ((f.exists && f.canWrite)|| f.createNewFile) {
      val fos = new FileOutputStream(f, mode)
      val bos = new BufferedOutputStream(fos)
      bytes.foreach(byte => bos.write(byte))
      bos.close
      true
    }
    false
  }

  def writeAppend(path: String, bytes: Array[Byte]): Boolean = this.writeOver(path, bytes, true)
}
