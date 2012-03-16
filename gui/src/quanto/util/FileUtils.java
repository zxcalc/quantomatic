/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author alex
 */
public final class FileUtils {
    private final static Logger logger = Logger.getLogger("quanto.util.FileUtils");
	private FileUtils() {}

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

    public static void copy(File fromFile, File toFile)
            throws IOException {
        if (!fromFile.isFile()) {
            throw new IOException("Not a file: " + fromFile.getPath());
        }
        if (toFile.exists() && !toFile.isFile()) {
            throw new IOException("Not a file: " + toFile.getPath());
        }

        FileInputStream from = null;
        FileOutputStream to = null;
        try {
            from = new FileInputStream(fromFile);
            to = new FileOutputStream(toFile);
            byte[] buffer = new byte[4096];
            int bytesRead;

            while ((bytesRead = from.read(buffer)) != -1) {
                to.write(buffer, 0, bytesRead); // write
            }
        } finally {
            if (from != null) {
                try {
                    from.close();
                } catch (IOException e) {
                    logger.log(Level.FINE, "Failed to close input file", e);
                }
            }
            if (to != null) {
                try {
                    to.close();
                } catch (IOException e) {
                    logger.log(Level.FINE, "Failed to close output file", e);
                }
            }
        }
    }
}
