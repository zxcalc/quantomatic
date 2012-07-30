package quanto.core.protocol.userdata.dataserialization;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */

//Identity

public class StringDataSerializer implements DataSerializer<String> {
    @Override
    public String fromString(String data) {
        return data;
    }

    @Override
    public String toString(String data) {
        return data;
    }
}
