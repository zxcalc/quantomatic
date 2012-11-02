/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.oldcore.protocol;

import quanto.util.LoggingInputStream;
import java.util.logging.Logger;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.logging.Level;
import static quanto.oldcore.protocol.Utils.*;

/**
 *
 * @author alex
 */
class ResponseReader {
    private static final char ESC = '\u001b';
    private final static Logger logger = Logger.getLogger("quanto.core.protocol");

    private LoggingInputStream input;
    private String version;
    private StringBuilder lastMessage = new StringBuilder();
    private String lastInvalidOutput;

    public String getLastInvalidOutput() {
        return lastInvalidOutput;
    }

	public String getLastMessage() {
		return lastMessage.toString();
	}

    public ResponseReader(InputStream input) {
        this.input = new LoggingInputStream(
                new BufferedInputStream(input), "quanto.core.protocol.stream");
    }
	
	public boolean isClosed() {
		return input == null;
	}

    public void close() throws IOException {
		if (!isClosed()) {
			input.close();
			input = null;
		}
    }
	
	/**
	 * Read a character from the stream.
	 * 
	 * Guarantees that it is not -1 (will throw IOException in that case).
	 * @return a character
	 * @throws IOException 
	 */
	private int read() throws IOException {
		if (isClosed()) {
			throw new IllegalStateException("Input stream is closed");
		}
		int gotCh = input.read();
		if (gotCh == -1) {
			input = null;
			throw new IOException("End of stream reached");
		}
		lastMessage.append(gotCh);
		return gotCh;
	}
	
	/**
	 * Read into a buffer.
	 * 
	 * Guarantees that length is not -1 (will throw IOException in that case).
	 * @return read count
	 */
	private int read(byte[] b) throws IOException {
		if (isClosed()) {
			throw new IllegalStateException("Input stream is closed");
		}
		int count = input.read(b);
		if (count == -1) {
			input = null;
			throw new IOException("End of stream reached");
		}
		lastMessage.append(b);
		return count;
	}
	
	/**
	 * Read into a buffer.
	 * 
	 * Guarantees that length is not -1 (will throw IOException in that case).
	 * @return read count
	 */
	private int read(byte[] b, int off, int len) throws IOException {
		if (isClosed()) {
			throw new IllegalStateException("Input stream is closed");
		}
		int count = input.read(b, off, len);
		if (count == -1) {
			input = null;
			throw new IOException("End of stream reached");
		}
		lastMessage.append(b);
		return count;
	}

    private void eatEsc() throws ProtocolException, IOException {
        int gotCh = read();
        if (gotCh != ESC) {
            if (Character.isISOControl(gotCh))
                throw new ProtocolException("Expected ESC from core, got \\u" + String.format("%1$04x", gotCh));
            else
                throw new ProtocolException("Expected ESC from core, got " + (char)gotCh);
        }
    }

    private void eatChar(char ch) throws ProtocolException, IOException {
        int gotCh = read();
        if (gotCh != ch) {
            if (Character.isISOControl(gotCh))
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
            pos += read(buffer, pos, buffer.length - pos);
        }

        eatEscChar(']');
        return buffer;
    }

    // I'm almost tempted to use a List<Byte> - Java makes this
    // painful to do efficiently
    private byte[] readToEscape() throws ProtocolException, IOException {
		if (isClosed()) {
			throw new IllegalStateException("Input stream is closed");
		}
        byte[] result = null;
        byte[] buffer = new byte[50];
        int escPos = -1;
        while (escPos == -1) {
            input.mark(buffer.length + 1);
            int count = read(buffer);
            for (int i = 0; i < count; ++i) {
                if (buffer[i] == ESC) {
                    byte next;
                    if (i + 1 < count)
                        next = buffer[i + 1];
                    else
                        next = (byte)read();
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
        read(buffer, 0, escPos);
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

    private void skipToBodyEnd() throws IOException, ProtocolException {
		if (isClosed()) {
			throw new IllegalStateException("Input stream is closed");
		}
        boolean esc = false;
        input.mark(2);
        int ch = read();
        while (true) {
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
            ch = read();
        }
    }

    private void readAvailableInvalidData() throws IOException {
		if (isClosed()) {
			return;
		}
		byte[] b = new byte[1024];
		int count = 0;
		int avail = input.available();
		while (avail > 0) {
			if (avail > b.length)
				avail = b.length;
			count = read(b, 0, avail);
			if (count != -1 && logger.isLoggable(Level.INFO)) {
				String strVal = new String(b, 0, count);
				logger.log(Level.INFO, "Discarding data: \"{0}\"",
						strVal.replace('\u001b', '\u00a4'));
				lastMessage.append(strVal);
			}
			avail = input.available();
		}
        lastInvalidOutput = lastMessage.toString();
    }
	
	private void eatMessageOpening() throws IOException, ProtocolException {
		eatEscChar('<');
		lastMessage.setLength(0);
		lastMessage.append(ESC);
		lastMessage.append('<');
	}

    public void waitForReady() throws IOException, ProtocolException {
        if (version != null)
            return;

        try {
			eatMessageOpening();
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

    private Response parseJsonResponseBody(String requestId) throws ProtocolException, IOException {
        Response resp = new Response(Response.MessageType.Json, requestId);
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

    private Response parseUnknownResponseBody(String code, String requestId) throws ProtocolException, IOException {
        skipToBodyEnd();
        Response resp = new Response(Response.MessageType.UnknownResponse, requestId);
        resp.setResponseCode(code);
        return resp;
    }

    public Response parseNextResponse() throws IOException, ProtocolException {
        waitForReady();
        try {
			eatMessageOpening();
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
            else if (code.equals("J"))
                resp = parseJsonResponseBody(requestId);
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
            readAvailableInvalidData();
            throw ex;
        } catch (ProtocolException ex) {
            input.writeLog(Level.SEVERE, "Received invalid message");
            readAvailableInvalidData();
            throw ex;
        }
    }
}
