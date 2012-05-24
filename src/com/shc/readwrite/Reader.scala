package com.shc.readwrite

import java.io.{ File, FileReader, FileInputStream }
import java.io.{ InputStream, BufferedInputStream, Reader, BufferedReader }
import java.io.IOException

class StringReader(path: String, bufferSize: Int) extends IterableReader[String] {

  protected val file = new File(path)

  override protected type readerType = BufferedReader
  override var open = file.exists && file.canRead
  override protected var continue = false
  override protected var reader: readerType = _
  override protected var temp: String = _
  override protected var buffer: String = _

  if (this.open) this.initIO()

  override protected def initIO() {
    this.reader = new BufferedReader(new FileReader(file), bufferSize)
    this.buffer = try { this.reader.readLine } catch { case _ => null }
    this.continue = if (this.buffer != null) true else false
  }

  override def next = {
    if (this.open && this.continue) {
      this.temp = this.buffer
      this.buffer = try {
        reader.readLine
      } catch {
        case e: Exception => null
      }
      if (this.buffer == null) this.continue = false
      this.temp
    } else ""
  }
}

class BytesReader(path: String, bufferSize: Int) extends IterableReader[Array[Byte]] {

  override protected type readerType = InputStream
  protected val file: File = new File(path)
  override var open = file.exists && file.canRead
  override protected var continue: Boolean = false
  override protected var reader: readerType = _
  override protected var buffer = new Array[Byte](bufferSize)
  override protected var temp: Array[Byte] = _

  protected var readSize: Int = 0

  if (this.open) this.initIO()

  override protected def initIO() {

    this.reader = new BufferedInputStream(new FileInputStream(this.file))
    this.readSize = try { reader.read(this.buffer, 0, bufferSize) } catch { case e: Exception => 0 }
    if (this.readSize > 0) this.continue = true

  }

  override def next = {
    if (this.open && this.continue) {
      this.temp = this.buffer.slice(0, this.readSize)
      this.readSize = try {
        this.reader.read(this.buffer, 0, this.bufferSize)
      } catch {
        case e: Exception => 0
      }
      if (this.readSize <= 0) this.continue = false
      this.temp
    } else Array[Byte]()
  }
}

class StreamReader(stream: InputStream, bufferSize: Int) extends IterableReader[Array[Byte]] {

  override protected type readerType = InputStream
  override protected var continue: Boolean = false
  override protected var reader: readerType = _
  override var open = try { stream.available; true } catch { case e: Exception => false }
  override protected var buffer = new Array[Byte](bufferSize)
  override protected var temp: Array[Byte] = _

  protected var readSize: Int = 0

  if (this.open) this.initIO()

  override protected def initIO() {
    this.reader = new BufferedInputStream(stream)
    this.readSize = try { reader.read(this.buffer, 0, bufferSize) } catch { case e: Exception => 0 }
    if (this.readSize > 0) this.continue = true
  }

  override def next = {
    if (this.open && this.continue) {
      this.temp = this.buffer.slice(0, this.readSize)
      this.readSize = try {
        this.reader.read(this.buffer, 0, this.bufferSize)
      } catch {
        case e: Exception => 0
      }
      if (this.readSize <= 0) this.continue = false
      this.temp
    } else Array[Byte]()
  }
}

trait IterableReader[T] extends scala.collection.Traversable[T] {

  // Override below
  protected type readerType <: java.io.Closeable
  var open: Boolean
  protected var continue: Boolean
  protected var reader: readerType
  protected var temp: T
  protected var buffer: T

  // execute this.initIO if this.open is true and bind reader and some other fiedls.
  protected def initIO: Unit

  def next: T

  def hasNext = {
    if (open && continue) true
    else {
      close
      false
    }
  }

  def close {
    if (open) {
      open = false
      reader.close
    }
  }

  def foreach[Unit](f: T => Unit) {
    while (hasNext)
      f(next)
  }
}
