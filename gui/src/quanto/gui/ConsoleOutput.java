/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

/**
 * A console output.
 *
 * @author alex
 */
public interface ConsoleOutput {
	/**
	 * Output a string to the console
	 * @param output The string to output
	 */
	void print(Object output);
	/**
	 * Output a string to the console, followed by a newline
	 *
	 * @param output The string to output
	 */
	void println(Object output);
	/**
	 * Output an error message to the console
	 *
	 * @param message The message to output
	 */
	void error(Object message);
}
