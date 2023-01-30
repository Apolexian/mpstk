// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.parser

import scala.util.parsing.combinator._
import scala.language.postfixOps

import mpstk.{Channel, Label, Role, Session, Type, GroundType}
import mpstk.{GlobalType, MPST, BasePayloadCont, PayloadCont,
              Branch, Select, Rec, RecVar, End}
import mpstk.Context
import mpstk.raw

protected[parser] case class ParserConfig[A, PC <: BasePayloadCont[A]](
  PayloadCont: (Type, A) => PC,
  endPayload: Type,
  endCont: A
)

/** Base parser trait with common syntactic elements. */
protected[parser]
abstract trait BaseParser extends RegexParsers {
  def comment: Parser[String] = """(?m)#.*$""".r
  def comments: Parser[Unit] = rep(comment) ^^ { _ => () }

  def identifier: Parser[String] = """[a-zA-Z]\w*""".r

  def label: Parser[Label] = identifier ^^ { l => Label(l) }
  def role:  Parser[Role]  = identifier ^^ { r => Role(r) }

  def ground: Parser[GroundType] = bool | int | string | unit | tcbinfo | errorDiffservSec | errorInsuffResources | errorConnectionExists | errorRemoteUnspecified | errorConnectionIllegal | errorConnectionDoesNotExist | socketFd | segRstSet | rstNotif | segAckSet | seg | segSynSet | segSynAckSet | data | segFinSet | segFinAckSet | close | connectionAborted | okServerReady | aPORDigest | okResp | errPermDenied | sTAT | lIST | rETR | dELE | nOOP | rSET | qUIT | okServerReadyTimestamp | uSER | pASS | okNameValid | errNameInvalid | errInvalidPass | errUnableToLock 

  def bool: Parser[GroundType.Bool.type] = "[Bb]ool".r ^^ {
    _ => GroundType.Bool
  }
  def int: Parser[GroundType.Int.type] = "[Ii]nt".r ^^ {
    _ => GroundType.Int
  }
  def string: Parser[GroundType.String.type] = {
    ("[Ss]tring".r | "[Ss]tr".r) ^^ { _ => GroundType.String }
  }
  def unit: Parser[GroundType.Unit.type] = "[Uu]nit".r ^^ {
    _ => GroundType.Unit
  }
  def tcbinfo: Parser[GroundType.TcbInfo.type] = "TcbInfo".r ^^ {
    _ => GroundType.TcbInfo
  }
  def errorDiffservSec: Parser[GroundType.ErrorDiffservSecurity.type] = "ErrorDiffservSecurity".r ^^ {
    _ => GroundType.ErrorDiffservSecurity
  }
  def errorInsuffResources: Parser[GroundType.ErrorInsufficientResources.type] = "ErrorInsufficientResources".r ^^ {
    _ => GroundType.ErrorInsufficientResources
  }
  def errorConnectionExists: Parser[GroundType.ErrorConnectionExists.type] = "ErrorConnectionExists".r ^^ {
    _ => GroundType.ErrorConnectionExists
  }
  def errorRemoteUnspecified: Parser[GroundType.ErrorRemoteUnspecified.type] = "ErrorRemoteUnspecified".r ^^ {
    _ => GroundType.ErrorRemoteUnspecified
  }
  def errorConnectionIllegal: Parser[GroundType.ErrorConnectionIllegal.type] = "ErrorConnectionIllegal".r ^^ {
    _ => GroundType.ErrorConnectionIllegal
  }
  def errorConnectionDoesNotExist: Parser[GroundType.ErrorConnectionDoesNotExist.type] = "ErrorConnectionDoesNotExist".r ^^ {
    _ => GroundType.ErrorConnectionDoesNotExist
  }
  def socketFd: Parser[GroundType.SocketFd.type] = "SocketFd".r ^^ {
    _ => GroundType.SocketFd
  }
  def segRstSet: Parser[GroundType.SegRstSet.type] = "SegRstSet".r ^^ {
    _ => GroundType.SegRstSet
  }
  def rstNotif: Parser[GroundType.RstNotif.type] = "RstNotif".r ^^ {
    _ => GroundType.RstNotif
  }
  def segAckSet: Parser[GroundType.SegAckSet.type] = "SegAckSet".r ^^ {
    _ => GroundType.SegAckSet
  }
  def seg: Parser[GroundType.SegDataCarrying.type] = "SegDataCarrying".r ^^ {
    _ => GroundType.SegDataCarrying
  }
  def segSynSet: Parser[GroundType.SegSynSet.type] = "SegSynSet".r ^^ {
    _ => GroundType.SegSynSet
  }
  def segSynAckSet: Parser[GroundType.SegSynAckSet.type] = "SegSynAckSet".r ^^ {
    _ => GroundType.SegSynAckSet
  }
  def data: Parser[GroundType.Data.type] = "Data".r ^^ {
    _ => GroundType.Data
  }
  def segFinSet: Parser[GroundType.SegFinSet.type] = "SegFinSet".r ^^ {
    _ => GroundType.SegFinSet
  }
  def segFinAckSet: Parser[GroundType.SegFinAckSet.type] = "SegFinAckSet".r ^^ {
    _ => GroundType.SegFinAckSet
  }
  def close: Parser[GroundType.Close.type] = "Close".r ^^ {
    _ => GroundType.Close
  }
  def connectionAborted: Parser[GroundType.ConnectionAborted.type] = "ConnectionAborted".r ^^ {
    _ => GroundType.ConnectionAborted
  }
  def okServerReady: Parser[GroundType.OkServerReady.type] = "OkServerReady".r ^^ {
    _ => GroundType.OkServerReady
  }
  def aPORDigest: Parser[GroundType.APORDigest.type] = "APORDigest".r ^^ {
    _ => GroundType.APORDigest
  }
  def okResp: Parser[GroundType.OkResp.type] = "OkResp".r ^^ {
    _ => GroundType.OkResp
  }
  def errPermDenied: Parser[GroundType.ErrPermDenied.type] = "ErrPermDenied".r ^^ {
    _ => GroundType.ErrPermDenied
  }
  def sTAT: Parser[GroundType.STAT.type] = "STAT".r ^^ {
    _ => GroundType.STAT
  }
  def lIST: Parser[GroundType.LIST.type] = "LIST".r ^^ {
    _ => GroundType.LIST
  }
  def rETR: Parser[GroundType.RETR.type] = "RETR".r ^^ {
    _ => GroundType.RETR
  }
  def dELE: Parser[GroundType.DELE.type] = "DELE".r ^^ {
    _ => GroundType.DELE
  }
  def nOOP: Parser[GroundType.NOOP.type] = "NOOP".r ^^ {
    _ => GroundType.NOOP
  }
  def rSET: Parser[GroundType.RSET.type] = "RSET".r ^^ {
    _ => GroundType.RSET
  }
  def qUIT: Parser[GroundType.QUIT.type] = "QUIT".r ^^ {
    _ => GroundType.QUIT
  }
  def okServerReadyTimestamp: Parser[GroundType.OkServerReadyTimestamp.type] = "OkServerReadyTimestamp".r ^^ {
    _ => GroundType.OkServerReadyTimestamp
  }
  def uSER: Parser[GroundType.USER.type] = "USER".r ^^ {
    _ => GroundType.USER
  }
  def pASS: Parser[GroundType.PASS.type] = "PASS".r ^^ {
    _ => GroundType.PASS
  }
  def okNameValid: Parser[GroundType.OkNameValid.type] = "OkNameValid".r ^^ {
    _ => GroundType.OkNameValid
  }
  def errNameInvalid: Parser[GroundType.ErrNameInvalid.type] = "ErrNameInvalid".r ^^ {
    _ => GroundType.ErrNameInvalid
  }
  def errInvalidPass: Parser[GroundType.ErrInvalidPass.type] = "ErrInvalidPass".r ^^ {
    _ => GroundType.ErrInvalidPass
  }
  def errUnableToLock: Parser[GroundType.ErrUnableToLock.type] = "ErrUnableToLock".r ^^ {
    _ => GroundType.ErrUnableToLock
  }
  def choice[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                             cont: Parser[A],
                             cfg: ParserConfig[A, PC]): Parser[(Label, PC)] = {
    label ~ payloadcont(tpe, cont, cfg) ^^ { lpc =>
      (lpc._1, lpc._2)
    }
  }

  def payloadcont[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    payloadcontFull(tpe, cont, cfg) |
    payloadcontNoPay(cont, cfg) |
    payloadcontNoCont(tpe, cfg) |
    payloadcontEmpty(cfg)
  }

  def payloadcontFull[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ~ ("." ~> cont) ^^ {
      pc => cfg.PayloadCont(pc._1, pc._2)
    }
  }

  def payloadcontNoPay[A, PC <: BasePayloadCont[A]](cont: Parser[A],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    (("(" ~ ")")?) ~> ("." ~> cont) ^^ { cnt =>
      cfg.PayloadCont(cfg.endPayload, cnt)
    }
  }

  def payloadcontNoCont[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                                      cfg: ParserConfig[A, PC]): Parser[PC] = {
    ("(" ~> tpe <~ ")") ^^ { pay =>
      cfg.PayloadCont(pay, cfg.endCont)
    }
  }

  def payloadcontEmpty[A, PC <: BasePayloadCont[A]](cfg: ParserConfig[A, PC]): Parser[PC] = {
    "" ^^ { _ => cfg.PayloadCont(cfg.endPayload, cfg.endCont) }
  }

  def choices[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choicesMulti(tpe, cont, cfg) | choicesSingle(tpe, cont, cfg)
  }

  def choicesMulti[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    "{" ~> rep1sep(choice(tpe, cont, cfg), ",") <~ "}" ^? ({ 
      case l: List[(Label, PC)] if (
        // Ensure that labels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val labels = l.map { _._1 }
      val dupl = labels.filter { l =>
        labels.indexOf(l) != labels.lastIndexOf(l)
      }.distinct
      f"""Choice with duplicated label(s): ${dupl.mkString(", ")}"""
    })
  }

  def choicesSingle[A, PC <: BasePayloadCont[A]](tpe: Parser[Type],
                       cont: Parser[A],
                       cfg: ParserConfig[A, PC]): Parser[List[(Label, PC)]] = {
    choice(tpe, cont, cfg) ^^ { c => List(c) }
  }
}

protected[parser]
class MPSTParser extends BaseParser {
  private val cfg = ParserConfig[MPST, PayloadCont](
    (payload: Type, cont: MPST) => PayloadCont(payload, cont),
    End,
    End
  )

  def tpe: Parser[Type] = ground | mpst

  // NOTE: always try to match recvar last (otw, it might capture e.g. "end")
  def mpst: Parser[MPST] = {
    ("(" ~> mpst <~ ")") | branch | select | end | rec | recvar
  }

  def closedmpst: Parser[MPST] = mpst ^? ({
    case t: MPST if t.closed => t
  }, { t =>
    f"The session must be closed (free variable(s): ${t.fv.mkString(", ")})"
  })

  def end: Parser[End.type] = "end".r ^^ { _ => End }

  def branchSym: Parser[String] = "&"
  def branch: Parser[Branch] = {
    role ~ (branchSym ~> choices(tpe, mpst, cfg)) ^^ { rc =>
      Branch(rc._1, Map(rc._2:_*))
    }
  }

  def selectSym: Parser[String] = "⊕" | "(+)" | "<+>"
  def select: Parser[Select] = {
    role ~ (selectSym ~> choices(tpe, mpst, cfg)) ^^ { rc =>
      Select(rc._1, Map(rc._2:_*))
    }
  }

  def recSym: Parser[String] = "μ" | "rec" | "mu"
  def rec: Parser[Rec] = (((recSym ~ "(") ~> recvar <~ ")") ~ mpst) ^^ { rm =>
    Rec(rm._1, rm._2)
  } ^? ({
    case t: Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def recvar: Parser[RecVar] = identifier ^^ { name => RecVar(name) }
}

/** Session type parser. */
object MPSTParser extends MPSTParser {
  /** Parse a session type from a string. */
  def parse(input: String): ParseResult[MPST] = {
    parseAll(comments ~> mpst, input)
  }
}

protected[parser]
class ContextParser extends MPSTParser {
  def session: Parser[Session] = identifier ^^ { s => Session(s) }

  def channel: Parser[Channel] = session ~ ("[" ~> role <~ "]") ^^ {
    sr => Channel(sr._1, sr._2)
  }

  def entry: Parser[(Channel, MPST)] = channel ~ (":" ~> closedmpst) ^^ { cs =>
    (cs._1, cs._2)
  }

  def entries: Parser[List[(Channel, MPST)]] = {
    repsep(entry, ",") ^? ({
      case l: List[(Channel, MPST)] if (
        // Ensure that channels are unique
        l.map(_._1).distinct.size == l.size
      ) => l
    }, { l =>
      val channels = l.map { _._1 }
      val dupl = channels.filter { c =>
        channels.indexOf(c) != channels.lastIndexOf(c)
      }.distinct
      f"""Context with duplicated channel(s): ${dupl.mkString(", ")}"""
    })
  }

  def context: Parser[Context] = entries ^^ { entries => Context(entries:_*) }
}

/** Parser for session typing contexts. */
object ContextParser extends ContextParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a session typing context from a string. */
  def parse(input: String): ParseResult[Context] = {
    parseAll(comments ~> context, input)
  }

  /** Parse a session typing context from a file, given as {@code Path}.
    * 
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[Context] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a session typing context from a file, given as {@code String}.
    * 
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[Context] = {
    parse(Paths.get(filename))
  }
}

protected[parser]
class GlobalTypeParser extends MPSTParser {
  private val cfg = ParserConfig[GlobalType, GlobalType.PayloadCont](
    (payload: Type, cont: GlobalType) => GlobalType.PayloadCont(payload, cont),
    End,
    GlobalType.End
  )

  private def payloadCont(payload: Type, cont: GlobalType) = {
    GlobalType.PayloadCont(payload, cont)
  }
  private val endPayload = End
  private val endCont = GlobalType.End

  // NOTE: always try to match gtrecvar last (otw, it might capture e.g. "end")
  def globaltype: Parser[GlobalType] = {
    ("(" ~> globaltype <~ ")") | comm | gtend | gtrec | gtrecvar
  }

  // We only accept closed types
  override def tpe: Parser[Type] = ground | closedmpst

  def gtend: Parser[GlobalType.End.type] = "end".r ^^ { _ => GlobalType.End }

  def commSym: Parser[String] = "→" | "->"
  def comm: Parser[GlobalType.Comm] = {
    role ~ (commSym ~> role <~ (":"?)) ~ choices(tpe, globaltype, cfg) ^^ { rc=>
      GlobalType.Comm(rc._1._1, rc._1._2, Map(rc._2:_*))
    }
  }

  // TODO: refactor the following, to avoid code duplication?
  def gtrecSym: Parser[String] = "μ" | "rec"
  def gtrec: Parser[GlobalType.Rec] = {
    (((gtrecSym ~ "(") ~> gtrecvar <~ ")") ~ globaltype) ^^ { rm =>
      GlobalType.Rec(rm._1, rm._2)
    }
  } ^? ({
    case t: GlobalType.Rec if t.guarded => t
  }, { t =>
    s"Unguarded recursion on ${t.recvar}"
  })

  def gtrecvar: Parser[GlobalType.RecVar] = identifier ^^ {
    name => GlobalType.RecVar(name)
  }
}

/** Parser for global types. */
object GlobalTypeParser extends GlobalTypeParser {
  import java.nio.file.{Files, Path, Paths}
  import scala.collection.JavaConverters._

  /** Parse a global type from a string. */
  def parse(input: String): ParseResult[GlobalType] = {
    parseAll(comments ~> globaltype, input)
  }

  /** Parse a global type from a file, given as {@code Path}.
    * 
    * @throws java.io.IOException in case of I/O error
    */
  def parse(input: Path): ParseResult[GlobalType] = {
    parse(Files.readAllLines(input).asScala.mkString("\n"))
  }

  /** Parse a global type from a file, given as {@code String}.
    * 
    * @throws java.io.IOException in case of I/O error
    * @throws java.nio.file.InvalidPathException if {@code filename} is invalid
    */
  def parseFile(filename: String): ParseResult[GlobalType] = {
    parse(Paths.get(filename))
  }
}
