/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 * @author alex
 */
public final class Util {
	private Util() {}
	public static String slurp(File file) throws IOException {
		BufferedReader r = new BufferedReader(new FileReader(file));
		try
		{
			StringBuilder result = new StringBuilder();
			int c = 0;
			while (c != -1) {
				c = r.read();
				result.append((char)c);
			}
			return result.toString();
		} finally {
			r.close();
		}
	}
}
