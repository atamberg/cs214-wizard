package apps.app64

import upickle.default.*
/**
  * The Stake class relates the amount of tricks a player won to the amount of tricks they bet
  *
  * @param tricksWon The amount of tricks won
  * @param bid The amount of tricks bet
  */
case class Stake(tricksWon: Int, bid: Int) derives ReadWriter:
  private val BasePoints = 20;
  private val BonusPoints = 10;
  /**
    * score returns the amount by which the score of a player changes at the end of a round
    * 
    * @return the amount of points lost/gained
    */
  def score =
    if tricksWon == bid then BasePoints + (tricksWon * BonusPoints)
    else math.abs(tricksWon - bid) * -BonusPoints

extension [K,V](m: Map[K,V])
  /** Update an element of the map at the given key by applying 'op' to it */
  def updateAtKey(k: K, op: V => V): Map[K,V] =
    m + (k -> (op(m(k))))

extension [K, T](m: Map[K, Seq[T]])
  /** For a map of sequences, append at the given key */
  def appendAtKey(k: K, toAppend: T): Map[K, Seq[T]] =
    m.updateAtKey(k, _ :+ toAppend)

  /** For a map that maps to sequences, prepend at the given key */
  def prependAtKey(k: K, toPrepend: T): Map[K, Seq[T]] =
    m.updateAtKey(k, toPrepend +: _)

extension [K, T](m: Map[K, Set[T]])
  /** For a map that maps to sets, drop all elements in toDrop from the set at the given key*/
  def dropAtKey(k: K, toDrop: T): Map[K, Set[T]] =
    m.updateAtKey(k, _ - toDrop)

