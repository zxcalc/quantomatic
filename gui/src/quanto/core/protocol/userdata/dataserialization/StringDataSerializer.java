package quanto.core.protocol.userdata.dataserialization;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */

//Identity

public class StringDataSerializer implements DataSerializer<String> {
    public String fromString(String data) {
        return data;
    }

    public String toString(String data) {
        return data;
    }
}
