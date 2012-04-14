package quanto.gui;

import com.sun.jaf.ui.ActionManager;
import java.util.Collections;
import java.util.LinkedList;

/**
 * Allows commands to be directed to the appropriate place
 *
 * These are the document-centric commands that will be passed
 * on to the document view.
 *
 * FIXME: this should do more (eg: methods to enable/disable actions
 * and trigger commands).  It should possibly do the multiplexing
 * instead of referring to ViewPort.
 *
 * @author Alex Merry
 */
public class CommandManager
{
    /**
     * Action name entries
     * 
     * Having these all in one place makes sure we don't spread typos around
     * the code.  Hence all other code should refer to
     * CommandManager.Command.Foo, not the string "foo-command".
     * 
     * This class also deals with registering the callbacks properly with
     * ActionManager.
     */
    public enum Command
    {
        Save("save-command"),
        SaveAs("save-as-command"),
        Undo("undo-command"),
        Redo("redo-command"),
        UndoRewrite("undo-rewrite-command"),
        RedoRewrite("redo-rewrite-command"),
        Cut("cut-command"),
        Copy("copy-command"),
        Paste("paste-command"),
        SelectAll("select-all-command"),
        DeselectAll("deselect-all-command"),
        //UseRule("use-rule-command"),

        Relayout("relayout-graph-command"),

	    Abort("abort-command"),
	    ExportToPdf("export-to-pdf-command"),
	    SelectMode("select-mode-command"),
	    DirectedEdgeMode("directed-edge-mode-command"),
	    UndirectedEdgeMode("undirected-edge-mode-command"),
	    LatexToClipboard("latex-to-clipboard-command"),
	    AddBoundaryVertex("add-boundary-vertex-command"),
	    ShowRewrites("show-rewrites-command"),
	    Normalise("normalise-command"),
	    FastNormalise("fast-normalise-command"),
	    BangVertices("bang-vertices-command"),
	    UnbangVertices("unbang-vertices-command"),
	    DropBangBox("drop-bang-box-command"),
	    KillBangBox("kill-bang-box-command"),
	    DuplicateBangBox("duplicate-bang-box-command"),
	    DumpHilbertTermAsText("hilbert-as-text-command"),
	    DumpHilbertTermAsMathematica("hilbert-as-mathematica-command");

        /**
         * Create a new command action
         * @param actionName  The action name (as in resources/actions.xml)
         */
        private Command(String actionName) {
            this.actionName = actionName;
        }
        private final String actionName;
        @Override
        public String toString() {
            return actionName;
        }
        public String actionName() {
            return actionName;
        }
        public boolean matches(String command) {
            return actionName.equals(command);
        }
    }

    public class Delegate {
        private Delegate() {
        }

        public void executeCommand(String command) {
            if (viewPort != null)
                viewPort.executeCommand(command);
        }

        public void executeCommand(String command, boolean state) {
            if (viewPort != null && state) {
                viewPort.executeCommand(command);
            }
        }
    }

    private Delegate delegate = new Delegate();
    private ActionManager actionManager;
    private ViewPort viewPort;

    public CommandManager(ActionManager actionManager) {
        this.actionManager = actionManager;
        LinkedList<String> actions = new LinkedList<String>();
        for (Command act: Command.values()) {
            actions.add(act.toString());
        }
        actionManager.registerGenericCallback(actions, delegate, "executeCommand");
    }

    public void registerCommand(String commandName) {
        actionManager.registerGenericCallback(
                Collections.<String>singleton(commandName),
                delegate,
                "executeCommand");
    }

    public void setViewPort(ViewPort viewPort) {
        this.viewPort = viewPort;
    }

    public ViewPort getViewPort() {
        return viewPort;
    }
}
