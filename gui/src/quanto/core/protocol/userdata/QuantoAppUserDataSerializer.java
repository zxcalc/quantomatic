package quanto.core.protocol.userdata;

import quanto.core.protocol.ProtocolManager;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class QuantoAppUserDataSerializer<T> {
    protected final String prefix = "quanto-gui:";
    protected ProtocolManager talker = null;
    public QuantoAppUserDataSerializer(ProtocolManager talker) {
        this.talker = talker;
    }
}
