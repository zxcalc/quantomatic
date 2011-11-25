package quanto.gui;

import com.sun.jaf.ui.ActionManager;
import java.util.Collections;
import java.util.LinkedList;

/**
 * Allows commands to be directed to the appropriate place
 * @author Alex Merry
 */
public class CommandManager
{
    public enum Command
    {
        Save("save-command"),
        SaveAs("save-as-command"),
        Undo("undo-command"),
        Redo("redo-command"),
        Cut("cut-command"),
        Copy("copy-command"),
        Paste("paste-command"),
        SelectAll("select-all-command"),
        DeselectAll("deselect-all-command");
        //UseRule("use-rule-command");

        private Command(String value) {
            this.actionName = value;
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
