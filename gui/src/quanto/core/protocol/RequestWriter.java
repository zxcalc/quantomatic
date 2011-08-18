/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

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

    public void addHeader(String code, String requestId) throws IOException
    {
        assert !inMessage;
        inMessage = true;
        addEscapedChar('<');
        output.write(convertAsciiString(code));
        addEscapedChar(':');
        output.write(convertUtf8String(requestId));
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
        addDataChunk(convertUtf8String(data));
    }

    public void addString(String data) throws IOException
    {
        assert inMessage;
        output.write(convertUtf8String(data));
    }
}
