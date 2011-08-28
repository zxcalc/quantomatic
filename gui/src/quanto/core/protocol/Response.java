/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import static quanto.core.protocol.Utils.*;

/**
 *
 * @author alex
 */
public class Response {
    public enum MessageType {
        Error,
        Ok,
        Console,
        ConsoleHelp,
        RawData,
        Pretty,
        Xml,
        Count,
        Name,
        NameList,
        UserData,
        StructuredData,
        UnknownRequest,
        UnknownResponse
    }
    private String requestId;
    private MessageType messageType;
    private String stringData;
    private String stringData2;
    private String[] stringListData;
    private byte[] byteData;
    private int intData;

    public Response(MessageType type, String requestId) {
        this.messageType = type;
        this.requestId = requestId;
    }

    public boolean isError() {
        return messageType == MessageType.Error;
    }

    public String getRequestId() {
        return requestId;
    }

    public MessageType getMessageType() {
        return messageType;
    }

    void setMessageType(MessageType messageType) {
        this.messageType = messageType;
    }

    public String getStringData() {
        assert messageType != MessageType.Error;
        assert stringData != null || byteData != null;

        if (stringData == null && byteData != null) {
            stringData = utf8ToString(byteData);
        }
        return stringData;
    }

    void setStringData(String stringData) {
        this.stringData = stringData;
    }

    public String[] getStringListData() {
        assert messageType != MessageType.Error;
        assert stringListData != null;

        return stringListData;
    }

    void setStringListData(String[] stringListData) {
        this.stringListData = stringListData;
    }

    public byte[] getByteData() {
        assert messageType != MessageType.Error;
        assert stringData != null || byteData != null;

        if (byteData == null && stringData != null) {
            byteData = stringToUtf8(stringData);
        }
        return byteData;
    }

    void setByteData(byte[] byteData) {
        this.byteData = byteData;
    }

    public int getIntData() {
        assert messageType == MessageType.Count;
        return intData;
    }

    void setIntData(int intData) {
        this.intData = intData;
    }

    public String getErrorCode() {
        assert messageType == MessageType.Error;
        return stringData;
    }

    void setErrorCode(String errorCode) {
        this.stringData = errorCode;
    }

    public String getErrorMessage() {
        assert messageType == MessageType.Error;
        return stringData2;
    }

    void setErrorMessage(String errorMessage) {
        this.stringData2 = errorMessage;
    }

    public String getCommandArgs() {
        assert messageType == MessageType.ConsoleHelp;
        return stringData;
    }

    void setCommandArgs(String commandArgs) {
        this.stringData = commandArgs;
    }

    public String getCommandHelp() {
        assert messageType == MessageType.ConsoleHelp;
        return stringData2;
    }

    void setCommandHelp(String commandHelp) {
        this.stringData2 = commandHelp;
    }

    public String getRequestCode() {
        assert messageType == MessageType.UnknownRequest;
        return stringData;
    }

    void setRequestCode(String code) {
        this.stringData = code;
    }

    public String getResponseCode() {
        switch (messageType) {
            case Console: return "C";
            case ConsoleHelp: return "H";
            case Count: return "I";
            case Error: return "Q";
            case Name: return "N";
            case NameList: return "M";
            case Ok: return "O";
            case Pretty: return "P";
            case RawData: return "R";
            case StructuredData: return "S";
            case UserData: return "U";
            case Xml: return "X";
            case UnknownRequest: return "Z";
            case UnknownResponse: return stringData;
        }
        throw new Error("Implement your damn function!");
    }

    void setResponseCode(String code) {
        assert messageType == MessageType.UnknownResponse;
        this.stringData = code;
    }
}
