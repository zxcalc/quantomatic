/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alemer
 */
class RequestBuilder
{
    public static final byte ESC = '\u001b';
    private LinkedList<byte[]> components = new LinkedList<byte[]>();
    private int messageLength = 0;
    private boolean complete = false;

    private void addHeader(byte[] code, byte[] requestId)
    {
        byte[] comp = new byte[6 + code.length + requestId.length];
        comp[0] = ESC;
        comp[1] = '<';
        int offset = 2;
        for (int i = 0; i < code.length; ++i, ++offset) {
            comp[offset] = code[i];
        }
        comp[offset] = ESC; ++offset;
        comp[offset] = ':'; ++offset;
        for (int i = 0; i < requestId.length; ++i, ++offset) {
            comp[offset] = requestId[i];
        }
        comp[offset] = ESC; ++offset;
        comp[offset] = '|'; ++offset;
        addComponent(comp);
    }

    private void addComponent(byte[] comp)
    {
        assert !complete;
        components.add(comp);
        messageLength += comp.length;
    }

    private byte[] convertInt(int i)
    {
        return stringToAscii(Integer.toString(i));
    }

    // only ASCII!!!
    public void addEscapedChar(char ch)
    {
        assert ch < 128;
        assert messageLength > 0;
        addComponent(new byte[] { ESC, (byte)ch });
    }

    public void addDelim()
    {
        assert messageLength > 0;
        addEscapedChar(';');
    }

    public void closeMessage()
    {
        assert messageLength > 0;
        addEscapedChar('>');
        complete = true;
    }

    public void addDataChunk(byte[] data)
    {
        assert messageLength > 0;
        addEscapedChar('[');
        addComponent(convertInt(data.length));
        addEscapedChar('|');
        addComponent(data);
        addEscapedChar(']');
    }

    public void addDataChunk(String data)
    {
        addDataChunk(stringToUtf8(data));
    }

    public void addString(String data)
    {
        addComponent(stringToUtf8(data));
    }

    public void addStringList(String[] items) throws IOException
    {
        addStringList(Arrays.asList(items));
    }

    public void addStringList(Collection<String> items) throws IOException
    {
        addComponent(convertInt(items.size()));
        addEscapedChar(':');
        boolean first = true;
        for (String item : items) {
            if (!first) {
                addEscapedChar(',');
            }
            addString(item);
            first = false;
        }
    }
}
