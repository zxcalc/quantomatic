/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.logging.Level;
import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alex
 */
public class ProtocolReader {
    private static final char ESC = '\u001b';

    private LoggingInputStream input;
    private String version;

    public ProtocolReader(InputStream input) {
        this.input = new LoggingInputStream(
                new BufferedInputStream(input), "quanto.core.protocol.stream");
    }

    public void close() throws IOException {
        input.close();
    }

    private void eatEsc() throws ProtocolException, IOException {
        int gotCh = input.read();
        if (gotCh != ESC) {
            if (gotCh == -1)
                throw new ProtocolException("Expected ESC from core, got EOF");
            else if (Character.isISOControl(gotCh))
                throw new ProtocolException("Expected ESC from core, got \\u" + String.format("%1$04x", gotCh));
            else
                throw new ProtocolException("Expected ESC from core, got " + (char)gotCh);
        }
    }

    private void eatChar(char ch) throws ProtocolException, IOException {
        int gotCh = input.read();
        if (gotCh != ch) {
            if (gotCh == -1)
                throw new ProtocolException("Expected " + ch + ", got EOF");
            else if (Character.isISOControl(gotCh))
                throw new ProtocolException("Expected " + ch + ", got \\u" + Integer.toHexString(gotCh));
            else
                throw new ProtocolException("Expected " + ch + ", got " + (char)gotCh);
        }
    }

    private void eatEscChar(char ch) throws ProtocolException, IOException {
        eatEsc();
        eatChar(ch);
    }

    private byte[] readDataBlock() throws ProtocolException, IOException {
        eatEscChar('[');
        int length = readIntToEscape();
        eatEscChar('|');

        byte[] buffer = new byte[length];
        int pos = 0;
        while (pos < buffer.length) {
            pos += input.read(buffer, pos, buffer.length - pos);
        }

        eatEscChar(']');
        return buffer;
    }

    // I'm almost tempted to use a List<Byte> - Java makes this
    // painful to do efficiently
    private byte[] readToEscape() throws ProtocolException, IOException {
        byte[] result = null;
        byte[] buffer = new byte[50];
        int escPos = -1;
        while (escPos == -1) {
            input.mark(buffer.length + 1);
            int count = input.read(buffer);
            for (int i = 0; i < count; ++i) {
                if (buffer[i] == ESC) {
                    byte next;
                    if (i + 1 < count)
                        next = buffer[i + 1];
                    else
                        next = (byte)input.read();
                    if (next == ESC) {
                        // escaped ESC
                        // shorten the array by one
                        for (int j = i + 1; j < count - 1; ++j) {
                            buffer[j] = buffer[j + 1];
                        }
                        --count;
                    } else {
                        escPos = i;
                        count = i;
                        break;
                    }
                }
            }
            if (result == null) {
                result = Arrays.copyOf(buffer, count);
            } else {
                byte[] newResult = Arrays.copyOf(result, result.length + count);
                for (int i = 0; i < count; ++i) {
                    newResult[result.length + i] = buffer[i];
                }
                result = newResult;
            }
        }
        input.reset();
        input.read(buffer, 0, escPos);
        return result;
    }

    private int readIntToEscape() throws ProtocolException, IOException {
        try {
            return Integer.parseInt(asciiToString(readToEscape()));
        } catch (NumberFormatException ex) {
            throw new ProtocolException("Expecting a decimal integer");
        }
    }

    private String readAsciiStringToEscape() throws ProtocolException, IOException {
        return asciiToString(readToEscape());
    }

    private String readStringToEscape() throws ProtocolException, IOException {
        return utf8ToString(readToEscape());
    }

    private String[] readStringList() throws ProtocolException, IOException {
        int length = readIntToEscape();
        if (length < 0)
            throw new ProtocolException("Array length cannot be negative");
        eatEscChar(':');
        String[] result = new String[length];
        for (int i = 0; i < result.length; ++i) {
            result[i] = readStringToEscape();
            if (i + 1 < result.length)
                eatEscChar(',');
        }
        return result;
    }

    public void waitForReady() throws IOException, ProtocolException {
        if (version != null)
            return;

        try {
            eatEscChar('<');
            eatChar('V');
            eatEscChar('|');
            version = readStringToEscape();
            eatEscChar('>');
            input.writeLog(Level.FINEST, "Received version message");
        } catch (IOException ex) {
            input.writeLog(Level.SEVERE, "Received partial version message");
            throw ex;
        } catch (ProtocolException ex) {
            input.writeLog(Level.SEVERE, "Received invalid version message");
            throw ex;
        }
    }

    public String getVersion() throws IOException, ProtocolException {
        waitForReady();
        return version;
    }

    private void eatDelim() throws IOException, ProtocolException {
        eatEscChar(';');
    }

    private Response parseErrorResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Error, requestId);
        resp.setErrorCode(readAsciiStringToEscape());
        eatDelim();
        resp.setErrorMessage(readStringToEscape());
        return resp;
    }

    private Response parseOkResponseBody(String requestId) throws ProtocolException, IOException {
        return new Response(Response.MessageType.Ok, requestId);
    }

    private Response parseConsoleResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Console, requestId);
        resp.setStringData(utf8ToString(readDataBlock()));
        return resp;
    }

    private Response parseConsoleHelpResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.ConsoleHelp, requestId);
        resp.setCommandArgs(readStringToEscape());
        eatDelim();
        resp.setCommandHelp(readStringToEscape());
        return resp;
    }

    private Response parseDataResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.RawData, requestId);
        resp.setByteData(readDataBlock());
        return resp;
    }

    private Response parsePrettyResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Pretty, requestId);
        resp.setStringData(utf8ToString(readDataBlock()));
        return resp;
    }

    private Response parseXmlResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Xml, requestId);
        resp.setStringData(utf8ToString(readDataBlock()));
        return resp;
    }

    private Response parseCountResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Count, requestId);
        resp.setIntData(readIntToEscape());
        return resp;
    }

    private Response parseNameResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Name, requestId);
        resp.setStringData(readStringToEscape());
        return resp;
    }

    private Response parseNameListResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.NameList, requestId);
        resp.setStringListData(readStringList());
        return resp;
    }

    private Response parseUserDataResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.UserData, requestId);
        resp.setByteData(readDataBlock());
        return resp;
    }

    private Response parseStructuredDataResponseBody(String requestId) throws ProtocolException, IOException {
        // ???
        skipToBodyEnd();
        throw new UnsupportedOperationException();
    }

    private Response parseUnknownRequestResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.UnknownRequest, requestId);
        resp.setRequestCode(readAsciiStringToEscape());
        return resp;
    }

    private void skipToBodyEnd() throws IOException, ProtocolException {
        boolean esc = false;
        input.mark(2);
        int ch = input.read();
        while (ch != -1) {
            if (esc) {
                if (ch == '[') {
                    // data chunk
                    input.reset();
                    readDataBlock();
                    input.mark(2);
                } else if (ch == '>') {
                    input.reset();
                    break;
                }
                esc = false;
                input.mark(2);
            } else if (ch == ESC) {
                esc = true;
            } else {
                input.mark(2);
            }
            ch = input.read();
        }
    }

    private Response parseUnknownResponseBody(String code, String requestId) throws ProtocolException, IOException {
        skipToBodyEnd();
        Response resp = new Response(Response.MessageType.UnknownResponse, requestId);
        resp.setResponseCode(code);
        return resp;
    }

    public Response parseNextResponse() throws IOException, ProtocolException {
        waitForReady();
        try {
            eatEscChar('<');
            String code = readAsciiStringToEscape();
            eatEscChar(':');
            String requestId = readStringToEscape();
            eatEscChar('|');
            Response resp;
            if (code.equals("Q"))
                resp = parseErrorResponseBody(requestId);
            else if (code.equals("O"))
                resp = parseOkResponseBody(requestId);
            else if (code.equals("C"))
                resp = parseConsoleResponseBody(requestId);
            else if (code.equals("H"))
                resp = parseConsoleHelpResponseBody(requestId);
            else if (code.equals("R"))
                resp = parseDataResponseBody(requestId);
            else if (code.equals("P"))
                resp = parsePrettyResponseBody(requestId);
            else if (code.equals("X"))
                resp = parseXmlResponseBody(requestId);
            else if (code.equals("I"))
                resp = parseCountResponseBody(requestId);
            else if (code.equals("N"))
                resp = parseNameResponseBody(requestId);
            else if (code.equals("M"))
                resp = parseNameListResponseBody(requestId);
            else if (code.equals("U"))
                resp = parseUserDataResponseBody(requestId);
            else if (code.equals("S"))
                resp = parseStructuredDataResponseBody(requestId);
            else if (code.equals("Z"))
                resp = parseUnknownRequestResponseBody(requestId);
            else
                resp = parseUnknownResponseBody(code, requestId);
            eatEscChar('>');
            input.writeLog(Level.FINEST, "Received message");
            return resp;
        } catch (IOException ex) {
            input.writeLog(Level.SEVERE, "Received partial message");
            throw ex;
        } catch (ProtocolException ex) {
            input.writeLog(Level.SEVERE, "Received invalid message");
            throw ex;
        }
    }
}
