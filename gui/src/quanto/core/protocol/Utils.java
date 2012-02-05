/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.core.protocol;

import java.io.UnsupportedEncodingException;
/**
 *
 * @author alex
 */
final class Utils {

    static byte[] stringToUtf8(String str) {
        try {
            return str.getBytes("UTF-8");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required UTF-8 encoding.");
        }
    }

    static String utf8ToString(byte[] bytes) {
        try {
            return new String(bytes, "UTF-8");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required UTF-8 encoding.");
        }
    }
    
    static byte[] stringToAscii(String str)
    {
        try {
            return str.getBytes("US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required US-ASCII encoding.");
        }
    }

    static String asciiToString(byte[] bytes) {
        try {
            return new String(bytes, "US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            throw new Error("The Java environment does not support the required US-ASCII encoding.");
        }
    }
}
