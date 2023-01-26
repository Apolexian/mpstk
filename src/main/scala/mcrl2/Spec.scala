// mpstk - the Multiparty Session Types toolKit
// Copyright 2018 Alceste Scalas <alceste.scalas @ imperial.ac.uk>
// Released under the MIT License: https://opensource.org/licenses/MIT
package mpstk.mcrl2

import mpstk.{Type, Session, Role, Channel, Label, GroundType}
import mpstk.raw.{MPST, Branch, Select, Rec, RecVar, Context, End}
import mpstk.util.resource

/** A map-like class generating fresh values for keys of type A.
  * 
  * The first time an unknown key is accessed, a fresh identifier is
  * created (of type {@code String}, using the given {@code prefix}
  * and {@code suffix}); the identifier is remembered, and returned
  * for future calls.
  * 
  * @param init initial contents for the map-like store
  */
protected[mcrl2] class FreshNames[A](val description: String,
                                     prefix: String, suffix: String,
                                     init: Map[A, String], base: Int) {
  // NOTE: the following map preserves insertion order when iterating
  val store = scala.collection.mutable.LinkedHashMap[A, String](init.toSeq:_*)
  var nextFreshId: Int = base // Used to remember the next available id

  /** Return an unique identifier for @x, remembering it for future calls */
  def apply(x:A): String = synchronized {
    if (store.contains(x)) store(x)
    else {
      val freshId = nextFreshId
      nextFreshId += 1
      val ret = s"${prefix}${freshId}${suffix}"
      store += (x -> ret)
      ret
    }
  }

  /** Return a legend of all autogenerated identifiers, skipping @init */
  def legend: String = synchronized {
    val exclude = init.keySet
    val ret = store.filterKeys(k => !exclude.contains(k)).map { kv =>
      s"% ${kv._2} => ${kv._1}\n"
    }.foldLeft("")(_ ++ _)
    if (ret.isEmpty) "% (none)\n" else ret
  }
}

object FreshNames {
  def apply[A](description: String,
               prefix: String, suffix: String): FreshNames[A] = {
    new FreshNames(description, prefix, suffix, Map.empty, 0)
  }

  def apply[A](description: String,
               prefix: String, suffix: String,
               init: Map[A, String], base: Int): FreshNames[A] = {
    new FreshNames(description, prefix, suffix, init, base)
  }
}

/** An mCRL2 process, constructed by the encoding procedure.
  * 
  * A process consists of a {@code current} part (being built by the
  * encoder),, and a series of process definitions ({@code defs}). The
  * two will be later merged in a single string, and then in a single
  * {@code .mcrl2} file
  */
protected[mcrl2]
case class Proc(current: List[String] = List(),
                defs: List[String] = List()) {
  /** Turn the {@code current} part into a sequential mCRL2 process */
  def sequential: Proc = Proc(
    // We need to ignore empty strings
    List(current.mkString(" . ")).filter(!_.isEmpty),
    defs
  )

  /** Compose two {@code Proc} instances, turning them into a choice
    * between sequential processes, and combining their definitions.
    */
  def +(that: Proc) = Proc(
    List({
           val cur = (this.sequential.current ++
                        that.sequential.current).mkString(" + ")
           // Add parentheses for correct precedence
           if (!cur.isEmpty) "(" ++ cur ++ ")" else cur
         }),
    defs ++ that.defs
  )

  /** Compose two specs, turning them into a parallel composition
    * of sequential mCRL2 processes, and combining their definitions.
    */
  def ||(that: Proc) = Proc(
    List(
      (this.sequential.current ++ that.sequential.current).mkString(" || ")
    ),
    defs ++ that.defs
  )

  /** Turn the {@code current} part into a sequential mCRL2 process
    * definition, and make it part of the {@code defs}.
    */
  def toDef(proc: String): Proc = {
    val procBody = {
      if (current.length == 0) "delta"
      else current.mkString(" . ")
    }
    Proc(
      List(),
      List(s"""proc ${proc} = ${procBody};""") ++ defs
    )
  }

  /** Represent the process specification as a string. */
  def show: String = {
    current.mkString(" %%% CURRENT\n") ++ "\n" ++ defs.mkString("\n\n")
  }

  /** Render the specification. This requires {@code current} to be empty! */
  def render: String = {
    assert(current.isEmpty)
    defs.mkString("\n\n")
  }
}


/** A mCRL2 specification, obtained from a typing context. */
class Spec(global: Option[mpstk.GlobalType], ctx: mpstk.Context,
           val description: String, val filename: String) {
  private val adaptedCtx = ctx.barendregt.unfoldPayloads.toRaw.addSubBranches

  /** Return the mCRL2 specification as a string */
  def show: String = show(true)
  def show(withLegend: Boolean): String = {
    // Access body first, to trigger its autogeneration and fill the legend
    val body2 = body

    preamble ++ "\n\n" ++
      (if (withLegend) legend else "") ++
      body2 ++ "\n\n" ++ init
  }

  /** Generate a legend for the autogenerated mCRL2 specification */
  private def legend: String = {
    // Iterate on all name suppliers  defined below
    val nameSuppliers = Seq(
      sessions, roles, labels, payloadTypes, entries, processes
    )

    def prettify[A](ctx: mpstk.ContextImpl[A]): String = ctx.map { kv =>
      s"% ${kv._1} -> ${kv._2}"
    }.mkString("\n")

    val gtype: String = global match {
      case None => "(none)"
      case Some(g) => g.toString
    }
    val gtypeDescr = s"%%% Original global type:\n% ${gtype}\n\n"

    val transforms: String = {
      s"%%% Original typing context:\n${prettify(ctx)}\n\n" ++
      s"%%% Adapted typing context:\n${prettify(adaptedCtx)}\n\n"
    }

    gtypeDescr ++ transforms ++ nameSuppliers.map { ns =>
      s"%%% ${ns.description}\n" ++ ns.legend ++ "\n"
    }.foldLeft(""){ _ ++ _ }
  }

  // Name suppliers for the mCRL2 spec encoding
  private val payloadTypes = FreshNames[Type]("Payload types",
                                              "p(", ")", Map[Type, String](
    // NOTE: must be kept in sync with resources/mpst-preamble.mcrl2
    mpstk.End                              -> "pEnd", 
    GroundType.Bool                        -> "pBool",
    GroundType.Int                         -> "pInt",
    GroundType.String                      -> "pString",
    GroundType.Unit                        -> "pUnit",
    GroundType.TcbInfo                     -> "pTcbInfo",
    GroundType.ErrorDiffservSecurity       -> "pEDiffservSec",
    GroundType.ErrorInsufficientResources  -> "pEResources",
    GroundType.ErrorConnectionExists       -> "pEConnectionExists",
    GroundType.ErrorRemoteUnspecified      -> "pERemoteUnspecified",
    GroundType.ErrorConnectionIllegal      -> "pEConnectionIllegal",
    GroundType.ErrorConnectionDoesNotExist -> "pEConnectionDoesNotExist",
    GroundType.SocketFd                    -> "pSocketFd",
    GroundType.SegRstSet                   -> "pSegRstSet",
    GroundType.RstNotif                    -> "pRstNotif",
    GroundType.SegAckSet                   -> "pSegAckSet",
    GroundType.SegDataCarrying             -> "pSegDataCarrying",
    GroundType.SegSynSet                   -> "pSegSynSet",
    GroundType.SegSynAckSet                -> "pSegSynAckSet",
    GroundType.Data                        -> "pData",
    GroundType.SegFinSet                   -> "pSegFinSet",
    GroundType.SegFinAckSet                -> "pSegFinAckSet",
    GroundType.Close                       -> "pClose",
    GroundType.ConnectionAborted           -> "pConnectionAborted"
  ), 25) // The base must be large enough to skip indexes for the above types
  private val labels = FreshNames[Label]("Message labels", "m(", ")")
  private val processes = FreshNames[(Channel, RecVar)](
    "Processes generated from multiparty session types recursion variables",
    "P", "")
  private val entries = FreshNames[(Channel, MPST)](
    "Processes representing typing context entries", "E", "")
  private val roles = FreshNames[Role]("Roles", "r(", ")")
  private val sessions = FreshNames[Session]("Sessions", "s(", ")")

  private lazy val preamble: String = resource("mcrl2/mpst-preamble.mcrl2")
  private lazy val init: String = resource("mcrl2/mpst-init.mcrl2")

  // Convert a typing context into a mCRL2 process spec
  private lazy val body: String = adaptedCtx.map { ct =>
    val entry = entries(ct)
    // Turn each entry into a definition, and call it
    mpstToSpec(ct._1, ct._2).toDef(entry) + Proc(List(entry))
  }.foldLeft(Proc()) { _ || _ }.toDef("context").render

  // Convert a session type for the given channel into a (sequential)
  // mCRL2 process spec
  private def mpstToSpec(chan: Channel, t: MPST): Proc = t match {
    case End => Proc()
    case Branch(from, choices) => choices.map { lpc =>
      val contSpec = mpstToSpec(chan, lpc._2.cont)
      Proc(
        List(s"i(${sessions(chan.session)}, ${roles(from)}, ${roles(chan.role)}, ${labels(lpc._1)}, ${payloadTypes(lpc._2.payload)})") ++ contSpec.current,
        contSpec.defs
      )
    }.foldLeft(Proc()) { _ + _ }
    case Select(to, choices) => choices.map { lpc =>
      val contSpec = mpstToSpec(chan, lpc._2.cont)
      Proc(
        List(s"o(${sessions(chan.session)}, ${roles(chan.role)}, ${roles(to)}, ${labels(lpc._1)}, ${payloadTypes(lpc._2.payload)})") ++ contSpec.current,
        contSpec.defs
      )
    }.foldLeft(Proc()) { _ + _ }
    case Rec(recvar, body) => {
      // NOTE: we exploit the Barendregt convention, to guarantee proc fresh
      val proc = processes((chan, recvar))
      // Encode the recursive process definition into dSpec...
      val dSpec = mpstToSpec(chan, body).toDef(proc)
      // ...and return a spec that calls the definition we just placed in dSpec
      Proc(List(proc)) + dSpec
    }
    case r @ RecVar(_) => Proc(List(s"${processes(chan, r)}"))
  }

  override val toString: String = s"Spec(${description})"
}

object Spec {
  /** Build an mCRL2 specification from the given typing context */
  def apply(ctx: mpstk.Context, description: String): Spec = {
    new Spec(None, ctx, description, "context")
  }

  /** Build (if possible) an mCRL2 specification from the given global type */
  def apply(global: mpstk.GlobalType,
            description: String): Either[String, Spec] = for {
    ctx <- global.context(Session("s"))
  } yield new Spec(Some(global), ctx, description, "context")
}
