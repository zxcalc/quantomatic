/*  Title:      Pure/PIDE/document.scala
    Author:     Makarius

Document as collection of named nodes, each consisting of an editable
list of commands, associated with asynchronous execution process.
*/

package isabelle


import scala.collection.mutable


object Document
{
  /** document structure **/

  /* overlays -- print functions with arguments */

  object Overlays
  {
    val empty = new Overlays(Map.empty)
  }

  final class Overlays private(rep: Map[Node.Name, Node.Overlays])
  {
    def apply(name: Document.Node.Name): Node.Overlays =
      rep.getOrElse(name, Node.Overlays.empty)

    private def update(name: Node.Name, f: Node.Overlays => Node.Overlays): Overlays =
    {
      val node_overlays = f(apply(name))
      new Overlays(if (node_overlays.is_empty) rep - name else rep + (name -> node_overlays))
    }

    def insert(command: Command, fn: String, args: List[String]): Overlays =
      update(command.node_name, _.insert(command, fn, args))

    def remove(command: Command, fn: String, args: List[String]): Overlays =
      update(command.node_name, _.remove(command, fn, args))

    override def toString: String = rep.mkString("Overlays(", ",", ")")
  }


  /* document blobs: auxiliary files */

  sealed case class Blob(bytes: Bytes, chunk: Symbol.Text_Chunk, changed: Boolean)
  {
    def unchanged: Blob = copy(changed = false)
  }

  object Blobs
  {
    def apply(blobs: Map[Node.Name, Blob]): Blobs = new Blobs(blobs)
    val empty: Blobs = apply(Map.empty)
  }

  final class Blobs private(blobs: Map[Node.Name, Blob])
  {
    def get(name: Node.Name): Option[Blob] = blobs.get(name)

    def changed(name: Node.Name): Boolean =
      get(name) match {
        case Some(blob) => blob.changed
        case None => false
      }

    override def toString: String = blobs.mkString("Blobs(", ",", ")")
  }


  /* document nodes: theories and auxiliary files */

  type Edit[A, B] = (Node.Name, Node.Edit[A, B])
  type Edit_Text = Edit[Text.Edit, Text.Perspective]
  type Edit_Command = Edit[Command.Edit, Command.Perspective]

  object Node
  {
    /* header and name */

    sealed case class Header(
      imports: List[(Name, Position.T)],
      keywords: Thy_Header.Keywords,
      errors: List[String])
    {
      def error(msg: String): Header = copy(errors = errors ::: List(msg))

      def cat_errors(msg2: String): Header =
        copy(errors = errors.map(msg1 => Library.cat_message(msg1, msg2)))
    }

    val no_header = Header(Nil, Nil, Nil)
    def bad_header(msg: String): Header = Header(Nil, Nil, List(msg))

    object Name
    {
      val empty = Name("")

      object Ordering extends scala.math.Ordering[Name]
      {
        def compare(name1: Name, name2: Name): Int = name1.node compare name2.node
      }
    }

    sealed case class Name(node: String, master_dir: String = "", theory: String = "")
    {
      override def hashCode: Int = node.hashCode
      override def equals(that: Any): Boolean =
        that match {
          case other: Name => node == other.node
          case _ => false
        }

      def is_theory: Boolean = theory.nonEmpty
      override def toString: String = if (is_theory) theory else node

      def map(f: String => String): Name = copy(f(node), f(master_dir), theory)
    }


    /* node overlays */

    object Overlays
    {
      val empty = new Overlays(Multi_Map.empty)
    }

    final class Overlays private(rep: Multi_Map[Command, (String, List[String])])
    {
      def commands: Set[Command] = rep.keySet
      def is_empty: Boolean = rep.isEmpty
      def dest: List[(Command, (String, List[String]))] = rep.iterator.toList
      def insert(cmd: Command, fn: String, args: List[String]): Overlays =
        new Overlays(rep.insert(cmd, (fn, args)))
      def remove(cmd: Command, fn: String, args: List[String]): Overlays =
        new Overlays(rep.remove(cmd, (fn, args)))

      override def toString: String = rep.mkString("Node.Overlays(", ",", ")")
    }


    /* edits */

    sealed abstract class Edit[A, B]
    {
      def foreach(f: A => Unit)
      {
        this match {
          case Edits(es) => es.foreach(f)
          case _ =>
        }
      }

      def is_void: Boolean =
        this match {
          case Edits(Nil) => true
          case _ => false
        }
    }
    case class Clear[A, B]() extends Edit[A, B]
    case class Blob[A, B](blob: Document.Blob) extends Edit[A, B]

    case class Edits[A, B](edits: List[A]) extends Edit[A, B]
    case class Deps[A, B](header: Header) extends Edit[A, B]
    case class Perspective[A, B](required: Boolean, visible: B, overlays: Overlays) extends Edit[A, B]


    /* perspective */

    type Perspective_Text = Perspective[Text.Edit, Text.Perspective]
    type Perspective_Command = Perspective[Command.Edit, Command.Perspective]

    val no_perspective_text: Perspective_Text =
      Perspective(false, Text.Perspective.empty, Overlays.empty)

    val no_perspective_command: Perspective_Command =
      Perspective(false, Command.Perspective.empty, Overlays.empty)

    def is_no_perspective_command(perspective: Perspective_Command): Boolean =
      !perspective.required &&
      perspective.visible.is_empty &&
      perspective.overlays.is_empty


    /* commands */

    object Commands
    {
      def apply(commands: Linear_Set[Command]): Commands = new Commands(commands)
      val empty: Commands = apply(Linear_Set.empty)

      def starts(commands: Iterator[Command], offset: Text.Offset = 0)
        : Iterator[(Command, Text.Offset)] =
      {
        var i = offset
        for (command <- commands) yield {
          val start = i
          i += command.length
          (command, start)
        }
      }

      private val block_size = 256
    }

    final class Commands private(val commands: Linear_Set[Command])
    {
      lazy val load_commands: List[Command] =
        commands.iterator.filter(cmd => cmd.blobs.nonEmpty).toList

      private lazy val full_index: (Array[(Command, Text.Offset)], Text.Range) =
      {
        val blocks = new mutable.ListBuffer[(Command, Text.Offset)]
        var next_block = 0
        var last_stop = 0
        for ((command, start) <- Commands.starts(commands.iterator)) {
          last_stop = start + command.length
          while (last_stop + 1 > next_block) {
            blocks += (command -> start)
            next_block += Commands.block_size
          }
        }
        (blocks.toArray, Text.Range(0, last_stop))
      }

      private def full_range: Text.Range = full_index._2

      def iterator(i: Text.Offset = 0): Iterator[(Command, Text.Offset)] =
      {
        if (commands.nonEmpty && full_range.contains(i)) {
          val (cmd0, start0) = full_index._1(i / Commands.block_size)
          Node.Commands.starts(commands.iterator(cmd0), start0) dropWhile {
            case (cmd, start) => start + cmd.length <= i }
        }
        else Iterator.empty
      }
    }

    val empty: Node = new Node()
  }

  final class Node private(
    val get_blob: Option[Document.Blob] = None,
    val header: Node.Header = Node.no_header,
    val syntax: Option[Prover.Syntax] = None,
    val text_perspective: Text.Perspective = Text.Perspective.empty,
    val perspective: Node.Perspective_Command = Node.no_perspective_command,
    _commands: Node.Commands = Node.Commands.empty)
  {
    def is_empty: Boolean =
      get_blob.isEmpty &&
      header == Node.no_header &&
      text_perspective.is_empty &&
      Node.is_no_perspective_command(perspective) &&
      commands.isEmpty

    def has_header: Boolean = header != Node.no_header

    def commands: Linear_Set[Command] = _commands.commands
    def load_commands: List[Command] = _commands.load_commands

    def clear: Node = new Node(header = header, syntax = syntax)

    def init_blob(blob: Document.Blob): Node = new Node(get_blob = Some(blob.unchanged))

    def update_header(new_header: Node.Header): Node =
      new Node(get_blob, new_header, syntax, text_perspective, perspective, _commands)

    def update_syntax(new_syntax: Option[Prover.Syntax]): Node =
      new Node(get_blob, header, new_syntax, text_perspective, perspective, _commands)

    def update_perspective(
        new_text_perspective: Text.Perspective,
        new_perspective: Node.Perspective_Command): Node =
      new Node(get_blob, header, syntax, new_text_perspective, new_perspective, _commands)

    def edit_perspective: Node.Edit[Text.Edit, Text.Perspective] =
      Node.Perspective(perspective.required, text_perspective, perspective.overlays)

    def same_perspective(
        other_text_perspective: Text.Perspective,
        other_perspective: Node.Perspective_Command): Boolean =
      text_perspective == other_text_perspective &&
      perspective.required == other_perspective.required &&
      perspective.visible.same(other_perspective.visible) &&
      perspective.overlays == other_perspective.overlays

    def update_commands(new_commands: Linear_Set[Command]): Node =
      if (new_commands eq _commands.commands) this
      else
        new Node(get_blob, header, syntax, text_perspective, perspective,
          Node.Commands(new_commands))

    def command_iterator(i: Text.Offset = 0): Iterator[(Command, Text.Offset)] =
      _commands.iterator(i)

    def command_iterator(range: Text.Range): Iterator[(Command, Text.Offset)] =
      command_iterator(range.start) takeWhile { case (_, start) => start < range.stop }

    def command_start(cmd: Command): Option[Text.Offset] =
      Node.Commands.starts(commands.iterator).find(_._1 == cmd).map(_._2)
  }


  /* development graph */

  object Nodes
  {
    val empty: Nodes = new Nodes(Graph.empty(Node.Name.Ordering))
  }

  final class Nodes private(graph: Graph[Node.Name, Node])
  {
    def apply(name: Node.Name): Node =
      graph.default_node(name, Node.empty).get_node(name)

    def is_hidden(name: Node.Name): Boolean =
    {
      val graph1 = graph.default_node(name, Node.empty)
      graph1.is_maximal(name) && graph1.get_node(name).is_empty
    }

    def + (entry: (Node.Name, Node)): Nodes =
    {
      val (name, node) = entry
      val imports = node.header.imports.map(_._1)
      val graph1 =
        (graph.default_node(name, Node.empty) /: imports)((g, p) => g.default_node(p, Node.empty))
      val graph2 = (graph1 /: graph1.imm_preds(name))((g, dep) => g.del_edge(dep, name))
      val graph3 = (graph2 /: imports)((g, dep) => g.add_edge(dep, name))
      new Nodes(
        if (graph3.is_maximal(name) && node.is_empty) graph3.del_node(name)
        else graph3.map_node(name, _ => node)
      )
    }

    def iterator: Iterator[(Node.Name, Node)] =
      graph.iterator.map({ case (name, (node, _)) => (name, node) })

    def load_commands(file_name: Node.Name): List[Command] =
      (for {
        (_, node) <- iterator
        cmd <- node.load_commands.iterator
        name <- cmd.blobs_names.iterator
        if name == file_name
      } yield cmd).toList

    def descendants(names: List[Node.Name]): List[Node.Name] = graph.all_succs(names)
    def topological_order: List[Node.Name] = graph.topological_order

    override def toString: String = topological_order.mkString("Nodes(", ",", ")")
  }



  /** versioning **/

  /* particular document versions */

  object Version
  {
    val init: Version = new Version()
    def make(nodes: Nodes): Version = new Version(Document_ID.make(), nodes)
  }

  final class Version private(
    val id: Document_ID.Version = Document_ID.none,
    val nodes: Nodes = Nodes.empty)
  {
    override def toString: String = "Version(" + id + ")"
  }


  /* changes of plain text, eventually resulting in document edits */

  object Change
  {
    val init: Change = new Change()

    def make(previous: Future[Version], edits: List[Edit_Text], version: Future[Version]): Change =
      new Change(Some(previous), edits.reverse, version)
  }

  final class Change private(
    val previous: Option[Future[Version]] = Some(Future.value(Version.init)),
    val rev_edits: List[Edit_Text] = Nil,
    val version: Future[Version] = Future.value(Version.init))
  {
    def is_finished: Boolean =
      (previous match { case None => true case Some(future) => future.is_finished }) &&
      version.is_finished

    def truncate: Change = new Change(None, Nil, version)
  }


  /* history navigation */

  object History
  {
    val init: History = new History()
  }

  final class History private(
    val undo_list: List[Change] = List(Change.init))  // non-empty list
  {
    def tip: Change = undo_list.head
    def + (change: Change): History = new History(change :: undo_list)

    def prune(check: Change => Boolean, retain: Int): Option[(List[Change], History)] =
    {
      val n = undo_list.iterator.zipWithIndex.find(p => check(p._1)).get._2 + 1
      val (retained, dropped) = undo_list.splitAt(n max retain)

      retained.splitAt(retained.length - 1) match {
        case (prefix, List(last)) => Some(dropped, new History(prefix ::: List(last.truncate)))
        case _ => None
      }
    }
  }


  /* snapshot */

  object Snapshot
  {
    val init = State.init.snapshot()
  }

  abstract class Snapshot
  {
    val state: State
    val version: Version
    val is_outdated: Boolean

    def convert(i: Text.Offset): Text.Offset
    def revert(i: Text.Offset): Text.Offset
    def convert(range: Text.Range): Text.Range
    def revert(range: Text.Range): Text.Range

    val node_name: Node.Name
    val node: Node
    val load_commands: List[Command]
    def is_loaded: Boolean
    def eq_content(other: Snapshot): Boolean

    def cumulate[A](
      range: Text.Range,
      info: A,
      elements: Markup.Elements,
      result: List[Command.State] => (A, Text.Markup) => Option[A],
      status: Boolean = false): List[Text.Info[A]]

    def select[A](
      range: Text.Range,
      elements: Markup.Elements,
      result: List[Command.State] => Text.Markup => Option[A],
      status: Boolean = false): List[Text.Info[A]]
  }



  /** global state -- document structure, execution process, editing history **/

  type Assign_Update =
    List[(Document_ID.Command, List[Document_ID.Exec])]  // update of exec state assignment

  object State
  {
    class Fail(state: State) extends Exception

    object Assignment
    {
      val init: Assignment = new Assignment()
    }

    final class Assignment private(
      val command_execs: Map[Document_ID.Command, List[Document_ID.Exec]] = Map.empty,
      val is_finished: Boolean = false)
    {
      def check_finished: Assignment = { require(is_finished); this }
      def unfinished: Assignment = new Assignment(command_execs, false)

      def assign(update: Assign_Update): Assignment =
      {
        require(!is_finished)
        val command_execs1 =
          (command_execs /: update) {
            case (res, (command_id, exec_ids)) =>
              if (exec_ids.isEmpty) res - command_id
              else res + (command_id -> exec_ids)
          }
        new Assignment(command_execs1, true)
      }
    }

    val init: State =
      State().define_version(Version.init, Assignment.init).assign(Version.init.id, Nil)._2
  }

  final case class State private(
    /*reachable versions*/
    versions: Map[Document_ID.Version, Version] = Map.empty,
    /*inlined auxiliary files*/
    blobs: Set[SHA1.Digest] = Set.empty,
    /*static markup from define_command*/
    commands: Map[Document_ID.Command, Command.State] = Map.empty,
    /*dynamic markup from execution*/
    execs: Map[Document_ID.Exec, Command.State] = Map.empty,
    /*command-exec assignment for each version*/
    assignments: Map[Document_ID.Version, State.Assignment] = Map.empty,
    /*commands with markup produced by other commands (imm_succs)*/
    commands_redirection: Graph[Document_ID.Command, Unit] = Graph.long,
    /*explicit (linear) history*/
    history: History = History.init,
    /*intermediate state between remove_versions/removed_versions*/
    removing_versions: Boolean = false)
  {
    private def fail[A]: A = throw new State.Fail(this)

    def define_version(version: Version, assignment: State.Assignment): State =
    {
      val id = version.id
      copy(versions = versions + (id -> version),
        assignments = assignments + (id -> assignment.unfinished))
    }

    def define_blob(digest: SHA1.Digest): State = copy(blobs = blobs + digest)
    def defined_blob(digest: SHA1.Digest): Boolean = blobs.contains(digest)

    def define_command(command: Command): State =
    {
      val id = command.id
      copy(commands = commands + (id -> command.init_state))
    }

    def defined_command(id: Document_ID.Command): Boolean = commands.isDefinedAt(id)

    def find_command(version: Version, id: Document_ID.Generic): Option[(Node, Command)] =
      commands.get(id) orElse execs.get(id) match {
        case None => None
        case Some(st) =>
          val command = st.command
          val node = version.nodes(command.node_name)
          if (node.commands.contains(command)) Some((node, command)) else None
      }

    def the_version(id: Document_ID.Version): Version = versions.getOrElse(id, fail)
    def the_static_state(id: Document_ID.Command): Command.State = commands.getOrElse(id, fail)
    def the_dynamic_state(id: Document_ID.Exec): Command.State = execs.getOrElse(id, fail)
    def the_assignment(version: Version): State.Assignment = assignments.getOrElse(version.id, fail)

    private def self_id(st: Command.State)(id: Document_ID.Generic): Boolean =
      id == st.command.id ||
      (execs.get(id) match { case Some(st1) => st1.command.id == st.command.id case None => false })

    private def other_id(id: Document_ID.Generic)
      : Option[(Symbol.Text_Chunk.Id, Symbol.Text_Chunk)] = None
    /* FIXME
      (execs.get(id) orElse commands.get(id)).map(st =>
        ((Symbol.Text_Chunk.Id(st.command.id), st.command.chunk)))
    */

    private def redirection(st: Command.State): Graph[Document_ID.Command, Unit] =
      (commands_redirection /: st.markups.redirection_iterator)({ case (graph, id) =>
        graph.default_node(id, ()).default_node(st.command.id, ()).add_edge(id, st.command.id) })

    def accumulate(id: Document_ID.Generic, message: XML.Elem): (Command.State, State) =
    {
      execs.get(id) match {
        case Some(st) =>
          val new_st = st.accumulate(self_id(st), other_id _, message)
          val execs1 = execs + (id -> new_st)
          (new_st, copy(execs = execs1, commands_redirection = redirection(new_st)))
        case None =>
          commands.get(id) match {
            case Some(st) =>
              val new_st = st.accumulate(self_id(st), other_id _, message)
              val commands1 = commands + (id -> new_st)
              (new_st, copy(commands = commands1, commands_redirection = redirection(new_st)))
            case None => fail
          }
      }
    }

    def assign(id: Document_ID.Version, update: Assign_Update): (List[Command], State) =
    {
      val version = the_version(id)

      def upd(exec_id: Document_ID.Exec, st: Command.State)
          : Option[(Document_ID.Exec, Command.State)] =
        if (execs.isDefinedAt(exec_id)) None else Some(exec_id -> st)

      val (changed_commands, new_execs) =
        ((Nil: List[Command], execs) /: update) {
          case ((commands1, execs1), (command_id, exec)) =>
            val st = the_static_state(command_id)
            val command = st.command
            val commands2 = command :: commands1
            val execs2 =
              exec match {
                case Nil => execs1
                case eval_id :: print_ids =>
                  execs1 ++ upd(eval_id, st) ++
                    (for (id <- print_ids; up <- upd(id, command.empty_state)) yield up)
              }
            (commands2, execs2)
        }
      val new_assignment = the_assignment(version).assign(update)
      val new_state = copy(assignments = assignments + (id -> new_assignment), execs = new_execs)

      (changed_commands, new_state)
    }

    def is_assigned(version: Version): Boolean =
      assignments.get(version.id) match {
        case Some(assgn) => assgn.is_finished
        case None => false
      }

    def is_stable(change: Change): Boolean =
      change.is_finished && is_assigned(change.version.get_finished)

    def recent_finished: Change = history.undo_list.find(_.is_finished) getOrElse fail
    def recent_stable: Change = history.undo_list.find(is_stable) getOrElse fail
    def tip_stable: Boolean = is_stable(history.tip)
    def tip_version: Version = history.tip.version.get_finished

    def continue_history(
      previous: Future[Version],
      edits: List[Edit_Text],
      version: Future[Version]): State =
    {
      val change = Change.make(previous, edits, version)
      copy(history = history + change)
    }

    def remove_versions(retain: Int = 0): (List[Version], State) =
    {
      history.prune(is_stable, retain) match {
        case Some((dropped, history1)) =>
          val old_versions = dropped.map(change => change.version.get_finished)
          val removing = old_versions.nonEmpty
          val state1 = copy(history = history1, removing_versions = removing)
          (old_versions, state1)
        case None => fail
      }
    }

    def removed_versions(removed: List[Document_ID.Version]): State =
    {
      val versions1 = versions -- removed
      val assignments1 = assignments -- removed
      var blobs1 = Set.empty[SHA1.Digest]
      var commands1 = Map.empty[Document_ID.Command, Command.State]
      var execs1 = Map.empty[Document_ID.Exec, Command.State]
      for {
        (version_id, version) <- versions1.iterator
        command_execs = assignments1(version_id).command_execs
        (_, node) <- version.nodes.iterator
        command <- node.commands.iterator
      } {
        for ((_, digest) <- command.blobs_defined; if !blobs1.contains(digest))
          blobs1 += digest

        if (!commands1.isDefinedAt(command.id))
          commands.get(command.id).foreach(st => commands1 += (command.id -> st))

        for (exec_id <- command_execs.getOrElse(command.id, Nil)) {
          if (!execs1.isDefinedAt(exec_id))
            execs.get(exec_id).foreach(st => execs1 += (exec_id -> st))
        }
      }
      copy(
        versions = versions1,
        blobs = blobs1,
        commands = commands1,
        execs = execs1,
        commands_redirection = commands_redirection.restrict(commands1.isDefinedAt(_)),
        assignments = assignments1,
        removing_versions = false)
    }

    private def command_states_self(version: Version, command: Command)
      : List[(Document_ID.Generic, Command.State)] =
    {
      require(is_assigned(version))
      try {
        the_assignment(version).check_finished.command_execs.getOrElse(command.id, Nil)
          .map(id => id -> the_dynamic_state(id)) match {
            case Nil => fail
            case res => res
          }
      }
      catch {
        case _: State.Fail =>
          try { List(command.id -> the_static_state(command.id)) }
          catch { case _: State.Fail => List(command.id -> command.init_state) }
      }
    }

    def command_states(version: Version, command: Command): List[Command.State] =
    {
      val self = command_states_self(version, command)
      val others =
        if (commands_redirection.defined(command.id)) {
          (for {
            command_id <- commands_redirection.imm_succs(command.id).iterator
            (id, st) <- command_states_self(version, the_static_state(command_id).command)
            if !self.exists(_._1 == id)
          } yield (id, st)).toMap.valuesIterator.toList
        }
        else Nil
      self.map(_._2) ::: others.flatMap(_.redirect(command))
    }

    def command_results(version: Version, command: Command): Command.Results =
      Command.State.merge_results(command_states(version, command))

    def command_markup(version: Version, command: Command, index: Command.Markup_Index,
        range: Text.Range, elements: Markup.Elements): Markup_Tree =
      Command.State.merge_markup(command_states(version, command), index, range, elements)

    def markup_to_XML(version: Version, node: Node, elements: Markup.Elements): XML.Body =
      (for {
        command <- node.commands.iterator
        markup =
          command_markup(version, command, Command.Markup_Index.markup, command.range, elements)
        tree <- markup.to_XML(command.range, command.source, elements)
      } yield tree).toList

    // persistent user-view
    def snapshot(name: Node.Name = Node.Name.empty, pending_edits: List[Text.Edit] = Nil)
      : Snapshot =
    {
      val stable = recent_stable
      val latest = history.tip


      /* pending edits and unstable changes */

      val rev_pending_changes =
        for {
          change <- history.undo_list.takeWhile(_ != stable)
          (a, edits) <- change.rev_edits
          if a == name
        } yield edits

      val is_cleared = rev_pending_changes.exists({ case Node.Clear() => true case _ => false })
      val edits =
        if (is_cleared) Nil
        else
          (pending_edits /: rev_pending_changes)({
            case (edits, Node.Edits(es)) => es ::: edits
            case (edits, _) => edits
          })
      lazy val reverse_edits = edits.reverse

      new Snapshot
      {
        /* global information */

        val state = State.this
        val version = stable.version.get_finished
        val is_outdated = pending_edits.nonEmpty || latest != stable


        /* local node content */

        def convert(offset: Text.Offset) =
          if (is_cleared) 0 else (offset /: edits)((i, edit) => edit.convert(i))
        def revert(offset: Text.Offset) =
          if (is_cleared) 0 else (offset /: reverse_edits)((i, edit) => edit.revert(i))

        def convert(range: Text.Range) = range.map(convert(_))
        def revert(range: Text.Range) = range.map(revert(_))

        val node_name = name
        val node = version.nodes(name)

        val load_commands: List[Command] =
          if (node_name.is_theory) Nil
          else version.nodes.load_commands(node_name)

        val is_loaded: Boolean = node_name.is_theory || load_commands.nonEmpty

        def eq_content(other: Snapshot): Boolean =
        {
          def eq_commands(commands: (Command, Command)): Boolean =
          {
            val states1 = state.command_states(version, commands._1)
            val states2 = other.state.command_states(other.version, commands._2)
            states1.length == states2.length &&
            (states1 zip states2).forall({ case (st1, st2) => st1 eq_content st2 })
          }

          !is_outdated && !other.is_outdated &&
          node.commands.size == other.node.commands.size &&
          (node.commands.iterator zip other.node.commands.iterator).forall(eq_commands) &&
          load_commands.length == other.load_commands.length &&
          (load_commands zip other.load_commands).forall(eq_commands)
        }


        /* cumulate markup */

        def cumulate[A](
          range: Text.Range,
          info: A,
          elements: Markup.Elements,
          result: List[Command.State] => (A, Text.Markup) => Option[A],
          status: Boolean = false): List[Text.Info[A]] =
        {
          val former_range = revert(range).inflate_singularity
          val (chunk_name, command_iterator) =
            load_commands match {
              case command :: _ => (Symbol.Text_Chunk.File(node_name.node), Iterator((command, 0)))
              case _ => (Symbol.Text_Chunk.Default, node.command_iterator(former_range))
            }
          val markup_index = Command.Markup_Index(status, chunk_name)
          (for {
            (command, command_start) <- command_iterator
            chunk <- command.chunks.get(chunk_name).iterator
            states = state.command_states(version, command)
            res = result(states)
            markup_range <- (former_range - command_start).try_restrict(chunk.range).iterator
            markup = Command.State.merge_markup(states, markup_index, markup_range, elements)
            Text.Info(r0, a) <- markup.cumulate[A](markup_range, info, elements,
              {
                case (a, Text.Info(r0, b)) => res(a, Text.Info(convert(r0 + command_start), b))
              }).iterator
            r1 <- convert(r0 + command_start).try_restrict(range).iterator
          } yield Text.Info(r1, a)).toList
        }

        def select[A](
          range: Text.Range,
          elements: Markup.Elements,
          result: List[Command.State] => Text.Markup => Option[A],
          status: Boolean = false): List[Text.Info[A]] =
        {
          def result1(states: List[Command.State]): (Option[A], Text.Markup) => Option[Option[A]] =
          {
            val res = result(states)
            (_: Option[A], x: Text.Markup) =>
              res(x) match {
                case None => None
                case some => Some(some)
              }
          }
          for (Text.Info(r, Some(x)) <- cumulate(range, None, elements, result1 _, status))
            yield Text.Info(r, x)
        }


        /* output */

        override def toString: String =
          "Snapshot(node = " + node_name.node + ", version = " + version.id +
            (if (is_outdated) ", outdated" else "") + ")"
      }
    }
  }
}

