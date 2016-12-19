package objsets

import common._
import TweetReader._

/**
  * �����, �������������� �����.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
  * ������������ ��������� �������� ����`Tweet` � ����� ������ � �������� ������.
  * ������ ����� ������ ����� ���� ����� (��� `TweetSet`�). ����
  * ���������, ������� ������ �����������: ��� ������ ����� `b`, ��� �������� ��������� �����
  * ������, ��� ���� � `b`. �������� � ������ ��������� ������.
  *
  * �������� ��������, ��� ������ ��������� ��������� ��� ���������� ��� ����� (���
  * ����� ����� ������� ����� �� ������ ������, ��� ��� �����). �
  * ������ ���������� ��������� / ������� ������ ���������� �� ������ �����
  * (��. `def incl`). `TweetSet` �� ����� ��������� ��� ����� � ���������� �������
  * �� ��������� �������������.
  *
  *
  * ������������ ������������� ��������� � �������� �������� �������� � ���, ��� ��������
  * ����� ��������� ����� ������ ������. ���� �� ������, �� ������ ������ ������ � ���������[1],
  * �� ��� �� ����������� ��� ���� ����� ��������� ��� �������.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet {

  /**
    * ���� ����� ��������� �������� � ���������� ������������ ���� ���������
    * ��������� ��������� ��� �������� �������� �������.
    *
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, new Empty)

  /**
    * ��� ��������������� ����� ��� `filter` ������� �������� ���������������� �����
    */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
    * ���������� ����� `TweetSet` ������� �������� ������������ `TweetSet`�� `this` � `that`.
    *
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  def union(that: TweetSet): TweetSet

  /**
    * ���������� ���� � ���������� ����������� �������� �� ���������.
    *
    * ����� `mostRetweeted` �� ������ ��������� ������ ������ ����������
    * ���� `java.util.NoSuchElementException`.
    *
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  def mostRetweeted: Tweet

  def empty: Boolean
  /**
    * ���������� ������, ���������� ��� ����� ����� ���������, ������������� �� ���������� ��������
    * � ��������� �������. ������� �������, ������ ��������������� ������ ������
    * ����� ������������ ���������� ��������.
    *
    * ���������: ����� `remove` � TweetSet ����� ����� �������.
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  def descendingByRetweet: TweetList

  /**
    * ��������� ������ ��� �����������
    */

  /**
    * ���������� ����� `TweetSet` ������� �������� ��� �������� ��������� �
    * � ����� ������� `tweet` � ������ ���� �� ��� �� ��������� � �������� ���������.
    *
    * ���� `this.contains(tweet)`, ������������ ������� ���������.
    */
  def incl(tweet: Tweet): TweetSet

  /**
    * ���������� ����� `TweetSet`, �������� `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /**
    * ��������� ���������� ��`tweet` � ������ `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /**
    * ���� ����� ��������� ������� � ��������� �� � ������� �������� ���������
    */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = new Empty

  /**
    * ��������� ������ ��� �����������
    */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def empty: Boolean = true

  override def union(that: TweetSet): TweetSet = that

  override def mostRetweeted: Tweet = throw new NoSuchElementException

  override def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) new NonEmpty(elem, left.filterAcc(p, acc), right.filter(p))
    else right.filterAcc(p, left.filterAcc(p, acc))
  }


  /**
    * ��������� ������ ��� �����������
    */

  def mostRetweeted: Tweet = {
    lazy val leftMost = left.mostRetweeted
    lazy val rightMost = right.mostRetweeted

    if( !left.empty && leftMost.retweets > elem.retweets )
      if( !right.empty && rightMost.retweets > leftMost.retweets )
        rightMost
      else
        leftMost
    else if( !right.empty && rightMost.retweets > elem.retweets )
      rightMost
    else
      elem
  }


  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  /**
    * ���������� ����� `TweetSet` ������� �������� ������������ `TweetSet`�� `this` � `that`.
    *
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  override def union(that: TweetSet): TweetSet = {
    (left union (right union that)).incl(elem)
  }

  override def empty: Boolean = false;

  /**
    * ���������� ������, ���������� ��� ����� ����� ���������, ������������� �� ���������� ��������
    * � ��������� �������. ������� �������, ������ ��������������� ������ ������
    * ����� ������������ ���������� ��������.
    *
    * ���������: ����� `remove` � TweetSet ����� ����� �������.
    * ������: ����� �� �� ����������� ���� ����� �����, ��� ������� ��� �������� abstract
    * � ����������� � ����������?
    */
  def descendingByRetweet: TweetList = {
    new Cons(mostRetweeted, remove(mostRetweeted).descendingByRetweet)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tw => google.exists(e => tw.text.contains(e)))

  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tw => apple.exists(e => tw.text.contains(e)))

  /**
    * ������ ���� ������, ����������� �������� ����� ���� �� ������ apple ��� google,
    * ��������������� �� ���������� ��������.
    */
  lazy val trending: TweetList = (googleTweets.union(appleTweets)).descendingByRetweet
}

object Main extends App {
  // ������� �� ������ ����� ������
  GoogleVsApple.trending foreach println
}