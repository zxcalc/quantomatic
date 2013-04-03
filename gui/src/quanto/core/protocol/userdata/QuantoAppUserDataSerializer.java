package quanto.core.protocol.userdata;

import quanto.core.protocol.CoreTalker;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class QuantoAppUserDataSerializer<T> {
    protected String prefix = "quanto-gui:";
    protected CoreTalker talker = null;
    public QuantoAppUserDataSerializer(CoreTalker talker) {
        this.talker = talker;
    }
}
