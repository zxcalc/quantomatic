/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.UnsupportedEncodingException;
import java.util.LinkedList;

/**
 *
 * @author alemer
 */
public class RequestBuilder
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
    
    private byte[] convertAsciiString(String str)
    {
        try {
            return str.getBytes("US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required US-ASCII encoding.");
        }
    }
    
    private byte[] convertUtf8String(String str)
    {
        try {
            return str.getBytes("UTF-8");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required UTF-8 encoding.");
        }
    }

    private byte[] convertInt(int i)
    {
        return convertAsciiString(Integer.toString(i));
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
        addDataChunk(convertUtf8String(data));
    }

    public void addString(String data)
    {
        addComponent(convertUtf8String(data));
    }
}
