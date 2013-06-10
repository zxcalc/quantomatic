package quanto.core.protocol.userdata.dataserialization;

/**
 * User: benjaminfrot
 * Date: 7/21/12
 */
public class IntegerDataSerializer implements DataSerializer<Integer> {
    public Integer fromString(String data) {
        return Integer.parseInt(data);
    }

    public String toString(Integer data) {
        return Integer.toString(data);
    }
}
