/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.util.Collection;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alemer
 */
public class RequestWriter
{
    private OutputStream output;
    private boolean inMessage = false;
    public static final byte ESC = '\u001b';

    public RequestWriter(OutputStream output) {
        this.output = output;
    }

    private byte[] convertInt(int i)
    {
        return stringToAscii(Integer.toString(i));
    }

    public void addHeader(String code, String requestId) throws IOException
    {
        assert !inMessage;
        inMessage = true;
        addEscapedChar('<');
        output.write(stringToAscii(code));
        addEscapedChar(':');
        output.write(stringToUtf8(requestId));
        addEscapedChar('|');
    }

    // only ASCII!!!
    public void addEscapedChar(char ch) throws IOException
    {
        assert ch < 128;
        assert inMessage;
        output.write(ESC);
        output.write(ch);
    }

    public void addDelim() throws IOException
    {
        assert inMessage;
        addEscapedChar(';');
    }

    public void closeMessage() throws IOException
    {
        assert inMessage;
        addEscapedChar('>');
        inMessage = false;
        output.flush();
    }

    public void addDataChunk(byte[] data) throws IOException
    {
        assert inMessage;
        addEscapedChar('[');
        output.write(convertInt(data.length));
        addEscapedChar('|');
        output.write(data);
        addEscapedChar(']');
    }

    public void addDataChunk(String data) throws IOException
    {
        addDataChunk(stringToUtf8(data));
    }

    public void addString(String data) throws IOException
    {
        assert inMessage;
        output.write(stringToUtf8(data));
    }

    public void addStringList(String[] items) throws IOException
    {
        addStringList(Arrays.asList(items));
    }

    public void addStringList(Collection<String> items) throws IOException
    {
        assert inMessage;
        output.write(convertInt(items.size()));
        addEscapedChar(':');
        boolean first = true;
        for (String item : items) {
            if (!first) {
                addEscapedChar(',');
            }
            output.write(stringToUtf8(item));
            first = false;
        }
    }
}
