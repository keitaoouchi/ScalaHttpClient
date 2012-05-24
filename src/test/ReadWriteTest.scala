package test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import com.shc.readwrite._

class ReadWriteTest extends AssertionsForJUnit {
  
  val nonexists = "./test_data/nonexists.txt"
  val normalfile = "./test_data/test2write.txt"
  val nonwritable = "./test_data/nonwritable/test.txt"
    
  def existence(path: String) = new java.io.File(path).exists
  
  def arraCompare[T](arra1:Array[T], arra2:Array[T]):Boolean = {
    val size = arra1.length
    if(size != arra2.length) return false
    Range(0, size).forall(i => arra1(i) == arra2(i))
  }
  
  @Test
  def readTest() {
    val src = "./test_data/test.txt"
    val reader = ReadWrite.readBytes(src, bufferSize = 1)
    val result = new scala.collection.mutable.ArrayBuffer[Byte]()
    for(read <- reader) result.appendAll(read)
    assertEquals(new String(result.toArray), "123456789\n987654321")
    assertFalse(reader.open)
    
    val streader = ReadWrite.readString(src)
    val str = new scala.collection.mutable.StringBuilder()
    for(read <- streader) str.append(read)
    assertEquals(str.toString, "123456789987654321")
    assertFalse(streader.open)
    
    val itertest = ReadWrite.readString(src)
    val list = itertest.toList
    assertEquals(list(0), "123456789")
    assertEquals(list(1), "987654321")
    
    val conn = new java.net.URL("http://www.google.com").openConnection
    conn.connect
    val stream = conn.getInputStream
    val httpreader = ReadWrite.readStream(stream)
    assertTrue(httpreader.open)
    val bytes = new scala.collection.mutable.ArrayBuffer[Byte]()
    for(read <- httpreader) bytes.appendAll(read)
    //println(new String(bytes.toArray))
  }
    
  @Test
  def nonExistsFileTest() {
    assertFalse(existence(nonexists))
    var writer = ReadWrite.writeIter(nonexists)
    assertTrue(writer.open)
    assertTrue(existence(nonexists))
    writer.close
    assertFalse(writer.open)
    assertTrue(existence(nonexists))
    
    writer.chabudaiFlipping
    assertTrue(existence(nonexists))
    writer = ReadWrite.writeIter(nonexists)
    assertTrue(writer.open)
    writer.chabudaiFlipping
    assertFalse(writer.open)
    assertFalse(existence(nonexists))
    writer.close
  }
  
  @Test
  def basicInstructionTest() {
    var writer = ReadWrite.writeIter(normalfile)
    assertTrue(writer.open)
    //character would be written as byte
    val toWrite = Array('A', 'B', 'C')
    toWrite.foreach(c => writer.write(c))
    assertTrue(writer.byteHistory.forall(_ == 1))
    assertEquals(writer.writtenSize, 3)
    assertTrue(arraCompare(writer.revert(1), Array('C'.toByte)))
    assertTrue(arraCompare(writer.revert(2), Array('B'.toByte)))
    assertTrue(arraCompare(writer.revert(3), Array('A'.toByte)))
    
    //strings would be also written as byte
    val toWrite2 = "abc"
    writer.write(toWrite2)
    assertEquals(writer.byteHistory.length, 4)
    assertEquals(writer.byteHistory.first, 3)
    assertEquals(new String(writer.revert(1)), toWrite2)
    
    writer.clear
    assertEquals(writer.writtenSize, 0)
    //int once covert to string, then be written as byte.
    val toWrite3 = 100
    writer.write(toWrite3)
    assertEquals(toWrite3.toString, "100")
    writer.write(toWrite3)
    assertTrue(arraCompare(writer.revert(1), "100".getBytes))
    writer.clear
    
    val toWrite4 = "can you read?"
    writer.write(toWrite4)
    writer.save
    writer.close
    
    val readed = ReadWrite.read(normalfile)
    assertEquals(new String(readed.toArray), toWrite4)
  }
  
}