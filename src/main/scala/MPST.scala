// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk

/** Role. */
case class Role(name: String) {
  override def toString = name
}

/** Message label. */
case class Label(name: String) {
  override def toString = name
}

/** Session */
case class Session(name: String) {
  override def toString = s"${name}"
}

/** Channel (with role): pairs a session with the role being played within. */
case class Channel(session: Session, role: Role) {
  override def toString = s"${session}[${role}]"
}

/** Generic type (session type, or ground type). */
sealed abstract class Type {
  def :<(that: Type) = ops.subtypes(this, that)
  def :>(that: Type) = ops.subtypes(that, this)
  def =:=(that: Type) = (this :< that) && (this :> that)
}

/** Marker trait for types that cannot contain recursion variables. */
sealed trait NonRecursiveType extends Type

sealed abstract class GroundType extends NonRecursiveType

/** Namespace for ground types. */
object GroundType {
  case object Bool extends GroundType {
    override def toString = "bool"
  }
  case object Int extends GroundType {
    override def toString = "int"
  }
  case object String extends GroundType {
    override def toString = "string"
  }
  case object Unit extends GroundType {
    override def toString = "unit"
  }
  case object TcbInfo extends GroundType {
    override def toString = "TcbInfo"
  }
  case object ErrorDiffservSecurity extends GroundType {
    override def toString = "ErrorDiffservSecurity"
  }
  case object ErrorInsufficientResources extends GroundType {
    override def toString = "ErrorInsufficientResources"
  }
  case object ErrorConnectionExists extends GroundType {
    override def toString = "ErrorConnectionExists"
  }
  case object ErrorRemoteUnspecified extends GroundType {
    override def toString = "ErrorRemoteUnspecified"
  }
  case object ErrorConnectionIllegal extends GroundType {
    override def toString = "ErrorConnectionIllegal"
  }
  case object ErrorConnectionDoesNotExist extends GroundType {
    override def toString = "ErrorConnectionDoesNotExist"
  }
  case object SocketFd extends GroundType {
    override def toString = "SocketFd"
  }
  case object SegRstSet extends GroundType {
    override def toString = "SegRstSet"
  }
  case object RstNotif extends GroundType {
    override def toString = "RstNotif"
  }
  case object SegAckSet extends GroundType {
    override def toString = "SegAckSet"
  }
  case object SegDataCarrying extends GroundType {
    override def toString = "SegDataCarrying"
  }
  case object SegSynSet extends GroundType {
    override def toString = "SegSynSet"
  }
  case object SegSynAckSet extends GroundType {
    override def toString = "SegSynAckSet"
  }
  case object Data extends GroundType {
    override def toString = "Data"
  }
  case object SegFinSet extends GroundType {
    override def toString = "SegFinSet"
  }
  case object SegFinAckSet extends GroundType {
    override def toString = "SegFinAckSet"
  }
  case object Close extends GroundType {
    override def toString = "Close"
  }
  case object ConnectionAborted extends GroundType {
    override def toString = "ConnectionAborted"
  }
  case object OkServerReady extends GroundType {
    override def toString = "OkServerReady"
  }
  case object APORDigest extends GroundType {
    override def toString = "APORDigest"
  }
  case object OkResp extends GroundType {
    override def toString = "OkResp"
  }
  case object ErrPermDenied extends GroundType {
    override def toString = "ErrPermDenied"
  }
  case object STAT extends GroundType {
    override def toString = "STAT"
  }
  case object LIST extends GroundType {
    override def toString = "LIST"
  }
  case object RETR extends GroundType {
    override def toString = "RETR"
  }
  case object DELE extends GroundType {
    override def toString = "DELE"
  }
  case object NOOP extends GroundType {
    override def toString = "NOOP"
  }
  case object RSET extends GroundType {
    override def toString = "RSET"
  }
  case object QUIT extends GroundType {
    override def toString = "QUIT"
  }
  case object OkServerReadyTimestamp extends GroundType {
    override def toString = "OkServerReadyTimestamp"
  }
  case object USER extends GroundType {
    override def toString = "USER"
  }
  case object PASS extends GroundType {
    override def toString = "PASS"
  }
  case object OkNameValid extends GroundType {
    override def toString = "OkNameValid"
  }
  case object ErrNameInvalid extends GroundType {
    override def toString = "ErrNameInvalid"
  }
  case object ErrInvalidPass extends GroundType {
    override def toString = "ErrInvalidPass"
  }
  case object ErrUnableToLock extends GroundType {
    override def toString = "ErrUnableToLock"
  }
}


/** Multiparty session type. */
sealed abstract class MPST extends Type {
  /** Return a version of this type respecting the Barendregt convention */
  def barendregt: MPST = ops.barendregt(this)

  /** Is the type closed? */
  def closed: Boolean = ops.closed(this)

  /** Free variables. */
  def fv: Set[RecVar] = ops.fv(this)

  /** Does the type have guarded recursion variables, only? */
  def guarded: Boolean = ops.guarded(this)

  /** Merge (if possible) with the given type. */
  def merge(that: MPST): Either[String, MPST] = ops.merge(this, that)

  /** Return the potential outputs towards role @to, as pairs of label
    * and payload.
    */
  def outputs(to: Role): Set[(Label, Type)] = ops.outputs(this, to)

  /** Unfold payload types, ensuring they do not have variables bound by the
    * carrier type.
    */
  def unfoldPayloads: MPST = ops.unfoldPayloads(this)

  /** Convert to raw MPST. */
  protected[mpstk] def toRaw: raw.MPST = raw.ops.mpstToRaw(this)
}

/** Terminated session type */
case object End extends MPST with NonRecursiveType {
  override def toString = "end"
}

/** Session type payload and continuation */
case class PayloadCont(payload: Type,
                       cont: MPST) extends BasePayloadCont[MPST](payload, cont)

/** A generic choice, abstracting branching and selection types */
sealed abstract class Choice(val choices: Choices[PayloadCont]) extends MPST {
  override def toString = {
    val cs = choices.map { lpc => s"${lpc._1}${lpc._2}" }
    cs.mkString(", ")
  }
}

/** Branching type. */
case class Branch(from: Role,
                  override val choices: Choices[PayloadCont]) extends Choice(choices) {
  override def toString = s"${from}&{${super.toString}}"
}

/** Selection type. */
case class Select(to: Role,
                  override val choices: Choices[PayloadCont]) extends Choice(choices) {
  override def toString = s"${to}⊕{${super.toString}}"
}

/** Recursive type. */
case class Rec(recvar: RecVar, body: MPST) extends MPST {
  override def toString = s"μ(${recvar})${body}"
}

/** Recursion variable. */
case class RecVar(name: String) extends MPST {
  override def toString = s"${name}"
}
