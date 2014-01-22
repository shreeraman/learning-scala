package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val codeTable1 = List(('a', List(0)), ('b', List(1)))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("validate times") {
    assert(times(List('a', 'b', 'a')).exists(pair => pair._1 == 'a' && pair._2 == 2))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('t', 2))) === List(Leaf('t', 2)))
  }

  test("validate singleton") {
    assert(singleton(Nil) === false)
    assert(singleton(List(Leaf('a', 1))) === true)
    assert(singleton(List(Leaf('a', 1), Leaf('b', 2), Leaf('z', 3))) === false)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("validate until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("validate createCodeTree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(createCodeTree(string2Chars("etxtxxx")) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
  }

  test("validate decode") {
	  new TestTrees {
	    assert(decode(t1, List(0, 1)) === string2Chars("ab"))
	  }
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("validate codeBits") {
    new TestTrees {
      assert(codeBits(codeTable1)('a') === List(0))
      assert(codeBits(codeTable1)('b') === List(1))
      
    }
  }
  
  test("validate convert") {
    assert(convert(Leaf('a', 1)) === List(('a', Nil)))
    new TestTrees {
      assert(convert(t1) === List(('a', List(0)), ('b', List(1))))
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }
  
  test("validate mergeTable") {
    assert(mergeCodeTables(List(('a', Nil)), List(('b', Nil))) === List(('a', List(0)), ('b', List(1))))
    assert(mergeCodeTables(List(('a', List(0)), ('b', List(1))), List(('c', Nil))) === List(('a', List(0, 0)), ('b', List(0, 1)), ('c', List(1))))    
  }
  
  test("validate quickEncode") {
    new TestTrees {
      assert(quickEncode(t1)("abba".toList) === List(0, 1, 1, 0))
      assert(quickEncode(t2)("dabbad".toList) === List(1, 0, 0, 0, 1, 0, 1, 0, 0, 1))
    }
  }
}
