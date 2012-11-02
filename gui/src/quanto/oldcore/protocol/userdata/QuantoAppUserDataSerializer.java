package quanto.oldcore.protocol.userdata;

import quanto.oldcore.protocol.CoreTalker;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class QuantoAppUserDataSerializer<T> {
    protected final String prefix = "quanto-gui:";
    protected CoreTalker talker = null;
    public QuantoAppUserDataSerializer(CoreTalker talker) {
        this.talker = talker;
    }
}
