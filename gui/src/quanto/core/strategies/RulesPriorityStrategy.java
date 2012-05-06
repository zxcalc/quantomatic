package quanto.core.strategies;

import quanto.core.Core;
import quanto.core.CoreException;
import quanto.core.data.AttachedRewrite;
import quanto.core.data.CoreGraph;

import java.util.List;

public class RulesPriorityStrategy implements QuantoStrategy{
    String strategyName = "SimplePriorityStrategy";
    Core core = null;
    public RulesPriorityStrategy(Core core) {
        this.core = core;
    }

    public int getNext(List<AttachedRewrite<CoreGraph>> rws, CoreGraph graph) {
        //Get the priorities of all the applicable rules and apply the last one in case of equality
        int max = -100;
        int count = 0;
        int toApply = 0;
        for (AttachedRewrite<CoreGraph> rw : rws) {
            int priority = getRulePriority(rw.getRuleName());
            if (priority >= max) {
                max = priority;
                toApply = count;
            }
            count++;
        }
        return toApply;
    }

    public String getName() {
        return this.strategyName;
    }

    private int getRulePriority(String ruleName) {
        try {
            String prioStr = this.core.getTalker().ruleUserData(ruleName, "quanto-gui:priority");
            return  Integer.parseInt(prioStr);
        } catch (CoreException e) {
            //We could not get the priority for that rule... set it to 5
            return 5;
        }
    }
}
