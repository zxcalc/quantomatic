/** 
 * Library gives all the static utility methods from PApplet - 
 * you can extend this to have the same env as you would normally 
 * expect from processing. 
 * 
 * @author ldixon
 */

import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Toolkit;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import processing.core.PConstants;
import processing.core.PGraphics;


public class PLib implements PConstants {
	
	  /**
	   * Full name of the Java version (i.e. 1.5.0_11).
	   * Prior to 0125, this was only the first three digits.
	   */
	  public static final String javaVersionName =
	    System.getProperty("java.version");

	  /**
	   * Version of Java that's in use, whether 1.1 or 1.3 or whatever,
	   * stored as a float.
	   * <P>
	   * Note that because this is stored as a float, the values may
	   * not be <EM>exactly</EM> 1.3 or 1.4. Instead, make sure you're
	   * comparing against 1.3f or 1.4f, which will have the same amount
	   * of error (i.e. 1.40000001). This could just be a double, but
	   * since Processing only uses floats, it's safer for this to be a float
	   * because there's no good way to specify a double with the preproc.
	   */
	  public static final float javaVersion =
	    new Float(javaVersionName.substring(0, 3)).floatValue();

	  /**
	   * Current platform in use.
	   * <P>
	   * Equivalent to System.getProperty("os.name"), just used internally.
	   */
	  static public String platformName =
	    System.getProperty("os.name");

	  /**
	   * Current platform in use, one of the
	   * PConstants WINDOWS, MACOSX, MACOS9, LINUX or OTHER.
	   */
	  static public int platform;

	  static {
	    if (platformName.indexOf("Mac") != -1) {
	      platform = MACOSX;

	    } else if (platformName.indexOf("Windows") != -1) {
	      platform = WINDOWS;

	    } else if (platformName.equals("Linux")) {  // true for the ibm vm
	      platform = LINUX;

	    } else {
	      platform = OTHER;
	    }
	  }

	  /**
	   * Modifier flags for the shortcut key used to trigger menus.
	   * (Cmd on Mac OS X, Ctrl on Linux and Windows)
	   */
	  static public final int MENU_SHORTCUT =
	    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();

	  /**
	   * Message of the Exception thrown when size() is called the first time.
	   * <P>
	   * This is used internally so that setup() is forced to run twice
	   * when the renderer is changed. This is the only way for us to handle
	   * invoking the new renderer while also in the midst of rendering.
	   */
	  static public final String NEW_RENDERER = "new renderer";

	  /** When debugging headaches */
	  static final boolean THREAD_DEBUG = false;

	  /** Default width and height for applet when not specified */
	  static public final int DEFAULT_WIDTH = 100;
	  static public final int DEFAULT_HEIGHT = 100;

	  /**
	   * Minimum dimensions for the window holding an applet.
	   * This varies between platforms, Mac OS X 10.3 can do any height
	   * but requires at least 128 pixels width. Windows XP has another
	   * set of limitations. And for all I know, Linux probably lets you
	   * make windows with negative sizes.
	   */
	  static public final int MIN_WINDOW_WIDTH = 128;
	  static public final int MIN_WINDOW_HEIGHT = 128;

	  // this text isn't seen unless PApplet is used on its
	  // own and someone takes advantage of leechErr.. not likely
	  static public final String LEECH_WAKEUP = "Error while running applet.";
	  //public PrintStream leechErr;

	  // messages to send if attached as an external vm

	  /**
	   * Position of the upper-lefthand corner of the editor window
	   * that launched this applet.
	   */
	  static public final String ARGS_EDITOR_LOCATION = "--editor-location";

	  /**
	   * Location for where to position the applet window on screen.
	   * <P>
	   * This is used by the editor to when saving the previous applet
	   * location, or could be used by other classes to launch at a
	   * specific position on-screen.
	   */
	  static public final String ARGS_EXTERNAL = "--external";

	  static public final String ARGS_LOCATION = "--location";

	  static public final String ARGS_DISPLAY = "--display";

	  static public final String ARGS_BGCOLOR = "--bgcolor";

	  static public final String ARGS_PRESENT = "--present";

	  static public final String ARGS_STOP_COLOR = "--stop-color";

	  static public final String ARGS_HIDE_STOP = "--hide-stop";

	  /**
	   * Allows the user or PdeEditor to set a specific sketch folder path.
	   * <P>
	   * Used by PdeEditor to pass in the location where saveFrame()
	   * and all that stuff should write things.
	   */
	  static public final String ARGS_SKETCH_FOLDER = "--sketch-path";

	  /**
	   * Message from parent editor (when run as external) to quit.
	   */
	  //static public final char EXTERNAL_STOP = 's';

	  /**
	   * When run externally to a PdeEditor,
	   * this is sent by the applet when it quits.
	   */
	  //static public final String EXTERNAL_QUIT = "__QUIT__";
	  static public final String EXTERNAL_STOP = "__STOP__";

	  /**
	   * When run externally to a PDE Editor, this is sent by the applet
	   * whenever the window is moved.
	   * <P>
	   * This is used so that the editor can re-open the sketch window
	   * in the same position as the user last left it.
	   */
	  static public final String EXTERNAL_MOVE = "__MOVE__";

	  static final String ERROR_MAX = "Cannot use max() on an empty array.";
	  static final String ERROR_MIN = "Cannot use min() on an empty array.";


	  // during rev 0100 dev cycle, working on new threading model,
	  // but need to disable and go conservative with changes in order
	  // to get pdf and audio working properly first.
	  // for 0116, the CRUSTY_THREADS are being disabled to fix lots of bugs.
	  static final boolean CRUSTY_THREADS = false; //true;


	  /**
	   * Get the current millisecond time.
	   * <P>
	   * This is a function, rather than a variable, because it may
	   * change multiple times per frame.
	   */
	  static public int millis() {
	    return (int) (System.currentTimeMillis());
	  }

	  /** Seconds position of the current time. */
	  static public int second() {
	    return Calendar.getInstance().get(Calendar.SECOND);
	  }

	  /** Minutes position of the current time. */
	  static public int minute() {
	    return Calendar.getInstance().get(Calendar.MINUTE);
	  }

	  /**
	   * Hour position of the current time in international format (0-23).
	   * <P>
	   * To convert this value to American time: <BR>
	   * <PRE>int yankeeHour = (hour() % 12);
	   * if (yankeeHour == 0) yankeeHour = 12;</PRE>
	   */
	  static public int hour() {
	    return Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
	  }

	  /**
	   * Get the current day of the month (1 through 31).
	   * <P>
	   * If you're looking for the day of the week (M-F or whatever)
	   * or day of the year (1..365) then use java's Calendar.get()
	   */
	  static public int day() {
	    return Calendar.getInstance().get(Calendar.DAY_OF_MONTH);
	  }

	  /**
	   * Get the current month in range 1 through 12.
	   */
	  static public int month() {
	    // months are number 0..11 so change to colloquial 1..12
	    return Calendar.getInstance().get(Calendar.MONTH) + 1;
	  }

	  /**
	   * Get the current year.
	   */
	  static public int year() {
	    return Calendar.getInstance().get(Calendar.YEAR);
	  }


	  /**
	   * Attempt to open a file using the platform's shell.
	   */
	  static public void open(String filename) {
	    open(new String[] { filename });
	  }


	  static String openLauncher;

	  /**
	   * Launch a process using a platforms shell. This version uses an array
	   * to make it easier to deal with spaces in the individual elements.
	   * (This avoids the situation of trying to put single or double quotes
	   * around different bits).
	   */
	  static public Process open(String argv[]) {
	    String[] params = null;

	    if (platform == WINDOWS) {
	      // just launching the .html file via the shell works
	      // but make sure to chmod +x the .html files first
	      // also place quotes around it in case there's a space
	      // in the user.dir part of the url
	      params = new String[] { "cmd", "/c" };

	    } else if (platform == MACOSX) {
	      params = new String[] { "open" };

	    } else if (platform == LINUX) {
	      if (openLauncher == null) {
	        // Attempt to use gnome-open
	        try {
	          Process p = Runtime.getRuntime().exec(new String[] { "gnome-open" });
	          /*int result =*/ p.waitFor();
	          // Not installed will throw an IOException (JDK 1.4.2, Ubuntu 7.04)
	          openLauncher = "gnome-open";
	        } catch (Exception e) { }
	      }
	      if (openLauncher == null) {
	        // Attempt with kde-open
	        try {
	          Process p = Runtime.getRuntime().exec(new String[] { "kde-open" });
	          /*int result =*/ p.waitFor();
	          openLauncher = "kde-open";
	        } catch (Exception e) { }
	      }
	      if (openLauncher == null) {
	        System.err.println("Could not find gnome-open or kde-open, " +
	                           "the open() command may not work.");
	      }
	      if (openLauncher != null) {
	        params = new String[] { openLauncher };
	      }
	    //} else {  // give up and just pass it to Runtime.exec()
	      //open(new String[] { filename });
	      //params = new String[] { filename };
	    }
	    if (params != null) {
	      // If the 'open', 'gnome-open' or 'cmd' are already included
	      if (params[0].equals(argv[0])) {
	        // then don't prepend those params again
	        return exec(argv);
	      } else {
	        params = concat(params, argv);
	        return exec(params);
	      }
	    } else {
	      return exec(argv);
	    }
	  }


	  static public Process exec(String[] argv) {
	    try {
	      return Runtime.getRuntime().exec(argv);
	    } catch (Exception e) {
	      e.printStackTrace();
	      throw new RuntimeException("Could not open " + join(argv, ' '));
	    }
	  }

	    //////////////////////////////////////////////////////////////


	  static public void print(byte what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(boolean what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(char what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(int what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(float what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(String what) {
	    System.out.print(what);
	    System.out.flush();
	  }

	  static public void print(Object what) {
	    if (what == null) {
	      // special case since this does fuggly things on > 1.1
	      System.out.print("null");
	    } else {
	      System.out.println(what.toString());
	    }

	    /*
	      String name = what.getClass().getName();
	      if (name.charAt(0) == '[') {
	        switch (name.charAt(1)) {
	        case '[':
	          // don't even mess with multi-dimensional arrays (case '[')
	          // or anything else that's not int, float, boolean, char
	          System.out.print(what);
	          System.out.print(' ');
	          break;

	        case 'L':
	          // print a 1D array of objects as individual elements
	          Object poo[] = (Object[]) what;
	          for (int i = 0; i < poo.length; i++) {
	            System.out.print(poo[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'Z':  // boolean
	          boolean zz[] = (boolean[]) what;
	          for (int i = 0; i < zz.length; i++) {
	            System.out.print(zz[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'B':  // byte
	          byte bb[] = (byte[]) what;
	          for (int i = 0; i < bb.length; i++) {
	            System.out.print(bb[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'C':  // char
	          char cc[] = (char[]) what;
	          for (int i = 0; i < cc.length; i++) {
	            System.out.print(cc[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'I':  // int
	          int ii[] = (int[]) what;
	          for (int i = 0; i < ii.length; i++) {
	            System.out.print(ii[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'F':  // float
	          float ff[] = (float[]) what;
	          for (int i = 0; i < ff.length; i++) {
	            System.out.print(ff[i]);
	            System.out.print(' ');
	          }
	          break;

	        case 'D':  // double
	          double dd[] = (double[]) what;
	          for (int i = 0; i < dd.length; i++) {
	            System.out.print(dd[i]);
	            System.out.print(' ');
	          }
	          break;

	        default:
	          System.out.print(what);
	        }
	      } else {
	        System.out.print(what); //.toString());
	      }
	    */
	  }

	  //

	  static public void println() {
	    System.out.println();
	  }

	  //

	  static public void println(byte what) {
	    print(what); System.out.println();
	  }

	  static public void println(boolean what) {
	    print(what); System.out.println();
	  }

	  static public void println(char what) {
	    print(what); System.out.println();
	  }

	  static public void println(int what) {
	    print(what); System.out.println();
	  }

	  static public void println(float what) {
	    print(what); System.out.println();
	  }

	  static public void println(String what) {
	    print(what); System.out.println();
	  }

	  static public void println(Object what) {
	    if (what == null) {
	      // special case since this does fuggly things on > 1.1
	      System.out.println("null");

	    } else {
	      String name = what.getClass().getName();
	      if (name.charAt(0) == '[') {
	        switch (name.charAt(1)) {
	        case '[':
	          // don't even mess with multi-dimensional arrays (case '[')
	          // or anything else that's not int, float, boolean, char
	          System.out.println(what);
	          break;

	        case 'L':
	          // print a 1D array of objects as individual elements
	          Object poo[] = (Object[]) what;
	          for (int i = 0; i < poo.length; i++) {
	            if (poo[i] instanceof String) {
	              System.out.println("[" + i + "] \"" + poo[i] + "\"");
	            } else {
	              System.out.println("[" + i + "] " + poo[i]);
	            }
	          }
	          break;

	        case 'Z':  // boolean
	          boolean zz[] = (boolean[]) what;
	          for (int i = 0; i < zz.length; i++) {
	            System.out.println("[" + i + "] " + zz[i]);
	          }
	          break;

	        case 'B':  // byte
	          byte bb[] = (byte[]) what;
	          for (int i = 0; i < bb.length; i++) {
	            System.out.println("[" + i + "] " + bb[i]);
	          }
	          break;

	        case 'C':  // char
	          char cc[] = (char[]) what;
	          for (int i = 0; i < cc.length; i++) {
	            System.out.println("[" + i + "] '" + cc[i] + "'");
	          }
	          break;

	        case 'I':  // int
	          int ii[] = (int[]) what;
	          for (int i = 0; i < ii.length; i++) {
	            System.out.println("[" + i + "] " + ii[i]);
	          }
	          break;

	        case 'F':  // float
	          float ff[] = (float[]) what;
	          for (int i = 0; i < ff.length; i++) {
	            System.out.println("[" + i + "] " + ff[i]);
	          }
	          break;

	          /*
	        case 'D':  // double
	          double dd[] = (double[]) what;
	          for (int i = 0; i < dd.length; i++) {
	            System.out.println("[" + i + "] " + dd[i]);
	          }
	          break;
	          */

	        default:
	          System.out.println(what);
	        }
	      } else {  // not an array
	        System.out.println(what);
	      }
	    }
	  }

	  //

	  /*
	  // not very useful, because it only works for public (and protected?)
	  // fields of a class, not local variables to methods
	  public void printvar(String name) {
	    try {
	      Field field = getClass().getDeclaredField(name);
	      println(name + " = " + field.get(this));
	    } catch (Exception e) {
	      e.printStackTrace();
	    }
	  }
	  */


	  //////////////////////////////////////////////////////////////

	  // MATH

	  // lots of convenience methods for math with floats.
	  // doubles are overkill for processing applets, and casting
	  // things all the time is annoying, thus the functions below.


	  static public final float abs(float n) {
	    return (n < 0) ? -n : n;
	  }

	  static public final int abs(int n) {
	    return (n < 0) ? -n : n;
	  }

	  static public final float sq(float a) {
	    return a*a;
	  }

	  static public final float sqrt(float a) {
	    return (float)Math.sqrt(a);
	  }

	  static public final float log(float a) {
	    return (float)Math.log(a);
	  }

	  static public final float exp(float a) {
	    return (float)Math.exp(a);
	  }

	  static public final float pow(float a, float b) {
	    return (float)Math.pow(a, b);
	  }


	  static public final int max(int a, int b) {
	    return (a > b) ? a : b;
	  }

	  static public final float max(float a, float b) {
	    return (a > b) ? a : b;
	  }


	  static public final int max(int a, int b, int c) {
	    return (a > b) ? ((a > c) ? a : c) : ((b > c) ? b : c);
	  }

	  static public final float max(float a, float b, float c) {
	    return (a > b) ? ((a > c) ? a : c) : ((b > c) ? b : c);
	  }


	  /**
	   * Find the maximum value in an array.
	   * Throws an ArrayIndexOutOfBoundsException if the array is length 0.
	   * @param list the source array
	   * @return The maximum value
	   */
	  static public final int max(int[] list) {
	    if (list.length == 0) {
	      throw new ArrayIndexOutOfBoundsException(ERROR_MAX);
	    }
	    int max = list[0];
	    for (int i = 1; i < list.length; i++) {
	      if (list[i] > max) max = list[i];
	    }
	    return max;
	  }

	  /**
	   * Find the maximum value in an array.
	   * Throws an ArrayIndexOutOfBoundsException if the array is length 0.
	   * @param list the source array
	   * @return The maximum value, or Float.NaN if the array is length zero.
	   */
	  static public final float max(float[] list) {
	    if (list.length == 0) {
	      throw new ArrayIndexOutOfBoundsException(ERROR_MAX);
	    }
	    float max = list[0];
	    for (int i = 1; i < list.length; i++) {
	      if (list[i] > max) max = list[i];
	    }
	    return max;
	  }


	  static public final int min(int a, int b) {
	    return (a < b) ? a : b;
	  }

	  static public final float min(float a, float b) {
	    return (a < b) ? a : b;
	  }


	  static public final int min(int a, int b, int c) {
	    return (a < b) ? ((a < c) ? a : c) : ((b < c) ? b : c);
	  }

	  static public final float min(float a, float b, float c) {
	    return (a < b) ? ((a < c) ? a : c) : ((b < c) ? b : c);
	  }


	  /**
	   * Find the minimum value in an array.
	   * Throws an ArrayIndexOutOfBoundsException if the array is length 0.
	   * @param list the source array
	   * @return The minimum value
	   */
	  static public final int min(int[] list) {
	    if (list.length == 0) {
	      throw new ArrayIndexOutOfBoundsException(ERROR_MIN);
	    }
	    int min = list[0];
	    for (int i = 1; i < list.length; i++) {
	      if (list[i] < min) min = list[i];
	    }
	    return min;
	  }
	  /**
	   * Find the minimum value in an array.
	   * Throws an ArrayIndexOutOfBoundsException if the array is length 0.
	   * @param list the source array
	   * @return The minimum value
	   */
	  static public final float min(float[] list) {
	    if (list.length == 0) {
	      throw new ArrayIndexOutOfBoundsException(ERROR_MIN);
	    }
	    float min = list[0];
	    for (int i = 1; i < list.length; i++) {
	      if (list[i] < min) min = list[i];
	    }
	    return min;
	  }


	  static public final int constrain(int amt, int low, int high) {
	    return (amt < low) ? low : ((amt > high) ? high : amt);
	  }

	  static public final float constrain(float amt, float low, float high) {
	    return (amt < low) ? low : ((amt > high) ? high : amt);
	  }


	  static public final float sin(float angle) {
	    return (float)Math.sin(angle);
	  }

	  static public final float cos(float angle) {
	    return (float)Math.cos(angle);
	  }

	  static public final float tan(float angle) {
	    return (float)Math.tan(angle);
	  }


	  static public final float asin(float value) {
	    return (float)Math.asin(value);
	  }

	  static public final float acos(float value) {
	    return (float)Math.acos(value);
	  }

	  static public final float atan(float value) {
	    return (float)Math.atan(value);
	  }

	  static public final float atan2(float a, float b) {
	    return (float)Math.atan2(a, b);
	  }


	  static public final float degrees(float radians) {
	    return radians * RAD_TO_DEG;
	  }

	  static public final float radians(float degrees) {
	    return degrees * DEG_TO_RAD;
	  }


	  static public final int ceil(float what) {
	    return (int) Math.ceil(what);
	  }

	  static public final int floor(float what) {
	    return (int) Math.floor(what);
	  }

	  static public final int round(float what) {
	    return (int) Math.round(what);
	  }


	  static public final float mag(float a, float b) {
	    return (float)Math.sqrt(a*a + b*b);
	  }

	  static public final float mag(float a, float b, float c) {
	    return (float)Math.sqrt(a*a + b*b + c*c);
	  }


	  static public final float dist(float x1, float y1, float x2, float y2) {
	    return sqrt(sq(x2-x1) + sq(y2-y1));
	  }

	  static public final float dist(float x1, float y1, float z1,
	                                 float x2, float y2, float z2) {
	    return sqrt(sq(x2-x1) + sq(y2-y1) + sq(z2-z1));
	  }


	  static public final float lerp(float start, float stop, float amt) {
	    return start + (stop-start) * amt;
	  }

	  /**
	   * Normalize a value to exist between 0 and 1 (inclusive).
	   * Mathematically the opposite of lerp(), figures out what proportion
	   * a particular value is relative to start and stop coordinates.
	   */
	  static public final float norm(float value, float start, float stop) {
	    return (value - start) / (stop - start);
	  }

	  /**
	   * Convenience function to map a variable from one coordinate space
	   * to another. Equivalent to unlerp() followed by lerp().
	   */
	  static public final float map(float value,
	                                float istart, float istop,
	                                float ostart, float ostop) {
	    return ostart + (ostop - ostart) * ((value - istart) / (istop - istart));
	  }



	  //////////////////////////////////////////////////////////////

	  // PERLIN NOISE

	  // [toxi 040903]
	  // octaves and amplitude amount per octave are now user controlled
	  // via the noiseDetail() function.

	  // [toxi 030902]
	  // cleaned up code and now using bagel's cosine table to speed up

	  // [toxi 030901]
	  // implementation by the german demo group farbrausch
	  // as used in their demo "art": http://www.farb-rausch.de/fr010src.zip

	  static final int PERLIN_YWRAPB = 4;
	  static final int PERLIN_YWRAP = 1<<PERLIN_YWRAPB;
	  static final int PERLIN_ZWRAPB = 8;
	  static final int PERLIN_ZWRAP = 1<<PERLIN_ZWRAPB;
	  static final int PERLIN_SIZE = 4095;

	  static public File inputFile(Frame parent) {
	    return inputFile("Select a file...", parent);
	  }


	  /**
	   * static version of inputFile usable by external classes.
	   * <P>
	   * The parentFrame is the Frame that will guide the placement of
	   * the prompt window. If no Frame is available, just pass in null.
	   */
	  // can't be static because it wants a host component
	  static public File inputFile(String prompt, Frame parentFrame) {
	    if (parentFrame == null) parentFrame = new Frame();
	    FileDialog fd = new FileDialog(parentFrame, prompt, FileDialog.LOAD);
	    fd.setVisible(true);

	    String directory = fd.getDirectory();
	    String filename = fd.getFile();
	    if (filename == null) return null;
	    return new File(directory, filename);
	  }


	  static public File outputFile(Frame parentFrame) {
	    return outputFile("Save as...", parentFrame);
	  }


	  /**
	   * static version of outputFile usable by external classes.
	   * <P>
	   * The parentFrame is the Frame that will guide the placement of
	   * the prompt window. If no Frame is available, just pass in null.
	   */
	  static public File outputFile(String prompt, Frame parentFrame) {
	    if (parentFrame == null) parentFrame = new Frame();
	    FileDialog fd = new FileDialog(parentFrame, prompt, FileDialog.SAVE);
	    fd.setVisible(true);

	    String directory = fd.getDirectory();
	    String filename = fd.getFile();
	    if (filename == null) return null;
	    return new File(directory, filename);
	  }


	  /**
	   * I want to read lines from a file. And I'm still annoyed.
	   */
	  static public BufferedReader createReader(File file) {
	    try {
	      InputStream is = new FileInputStream(file);
	      if (file.getName().toLowerCase().endsWith(".gz")) {
	        is = new GZIPInputStream(is);
	      }
	      return createReader(is);

	    } catch (Exception e) {
	      if (file == null) {
	        throw new RuntimeException("File passed to createReader() was null");
	      } else {
	        e.printStackTrace();
	        throw new RuntimeException("Couldn't create a reader for " +
	                                   file.getAbsolutePath());
	      }
	    }
	    //return null;
	  }


	  /**
	   * I want to read lines from a stream. If I have to type the
	   * following lines any more I'm gonna send Sun my medical bills.
	   */
	  static public BufferedReader createReader(InputStream input) {
	    InputStreamReader isr = null;
	    try {
	      isr = new InputStreamReader(input, "UTF-8");
	    } catch (UnsupportedEncodingException e) { }  // not gonna happen
	    return new BufferedReader(isr);
	  }


	  /**
	   * I want to print lines to a file. I have RSI from typing these
	   * eight lines of code so many times.
	   */
	  static public PrintWriter createWriter(File file) {
	    try {
	      OutputStream output = new FileOutputStream(file);
	      if (file.getName().toLowerCase().endsWith(".gz")) {
	        output = new GZIPOutputStream(output);
	      }
	      return createWriter(output);

	    } catch (Exception e) {
	      if (file == null) {
	        throw new RuntimeException("File passed to createWriter() was null");
	      } else {
	        e.printStackTrace();
	        throw new RuntimeException("Couldn't create a writer for " +
	                                   file.getAbsolutePath());
	      }
	    }
	    //return null;
	  }


	  /**
	   * I want to print lines to a file. Why am I always explaining myself?
	   * It's the JavaSoft API engineers who need to explain themselves.
	   */
	  static public PrintWriter createWriter(OutputStream output) {
	    try {
	      OutputStreamWriter osw = new OutputStreamWriter(output, "UTF-8");
	      return new PrintWriter(osw);
	    } catch (UnsupportedEncodingException e) { }  // not gonna happen
	    return null;
	  }


	  static public InputStream createInput(File file) {
	    try {
	      InputStream input = new FileInputStream(file);
	      if (file.getName().toLowerCase().endsWith(".gz")) {
	        return new GZIPInputStream(input);
	      }
	      return input;

	    } catch (IOException e) {
	      if (file == null) {
	        throw new RuntimeException("File passed to openStream() was null");

	      } else {
	        e.printStackTrace();
	        throw new RuntimeException("Couldn't openStream() for " +
	                                   file.getAbsolutePath());
	      }
	    }
	  }


	  static public byte[] loadBytes(InputStream input) {
	    try {
	      BufferedInputStream bis = new BufferedInputStream(input);
	      ByteArrayOutputStream out = new ByteArrayOutputStream();

	      int c = bis.read();
	      while (c != -1) {
	        out.write(c);
	        c = bis.read();
	      }
	      return out.toByteArray();

	    } catch (IOException e) {
	      e.printStackTrace();
	      //throw new RuntimeException("Couldn't load bytes from stream");
	    }
	    return null;
	  }


	  static public String[] loadStrings(File file) {
	    InputStream is = createInput(file);
	    if (is != null) return loadStrings(is);
	    return null;
	  }


	  static public String[] loadStrings(InputStream input) {
	    try {
	      BufferedReader reader =
	        new BufferedReader(new InputStreamReader(input, "UTF-8"));

	      String lines[] = new String[100];
	      int lineCount = 0;
	      String line = null;
	      while ((line = reader.readLine()) != null) {
	        if (lineCount == lines.length) {
	          String temp[] = new String[lineCount << 1];
	          System.arraycopy(lines, 0, temp, 0, lineCount);
	          lines = temp;
	        }
	        lines[lineCount++] = line;
	      }
	      reader.close();

	      if (lineCount == lines.length) {
	        return lines;
	      }

	      // resize array to appropriate amount for these lines
	      String output[] = new String[lineCount];
	      System.arraycopy(lines, 0, output, 0, lineCount);
	      return output;

	    } catch (IOException e) {
	      e.printStackTrace();
	      //throw new RuntimeException("Error inside loadStrings()");
	    }
	    return null;
	  }



	  static public OutputStream createOutput(File file) { 
	    try {
	      return new FileOutputStream(file);

	    } catch (IOException e) {
	      e.printStackTrace();
	    }
	    return null;
	  }
	  
	  
	  static public void saveStream(File targetFile, InputStream sourceStream) {
	    File tempFile = null;
	    try {
	      File parentDir = targetFile.getParentFile();
	      tempFile = File.createTempFile(targetFile.getName(), null, parentDir);

	      BufferedInputStream bis = new BufferedInputStream(sourceStream, 16384);
	      FileOutputStream fos = new FileOutputStream(tempFile);
	      BufferedOutputStream bos = new BufferedOutputStream(fos);

	      byte[] buffer = new byte[8192];
	      int bytesRead;
	      while ((bytesRead = bis.read(buffer)) != -1) {
	        bos.write(buffer, 0, bytesRead);
	      }

	      bos.flush();
	      bos.close();
	      bos = null;

	      if (!tempFile.renameTo(targetFile)) {
	        System.err.println("Could not rename temporary file " +
	                           tempFile.getAbsolutePath());
	      }
	    } catch (IOException e) {
	      if (tempFile != null) {
	        tempFile.delete();
	      }
	      e.printStackTrace();
	    }
	  }


	  /**
	   * Saves bytes to a specific File location specified by the user.
	   */
	  static public void saveBytes(File file, byte buffer[]) {
	    try {
	      String filename = file.getAbsolutePath();
	      createPath(filename);
	      OutputStream output = new FileOutputStream(file);
	      if (file.getName().toLowerCase().endsWith(".gz")) {
	        output = new GZIPOutputStream(output);
	      }
	      saveBytes(output, buffer);
	      output.close();

	    } catch (IOException e) {
	      System.err.println("error saving bytes to " + file);
	      e.printStackTrace();
	    }
	  }


	  /**
	   * Spews a buffer of bytes to an OutputStream.
	   */
	  static public void saveBytes(OutputStream output, byte buffer[]) {
	    try {
	      output.write(buffer);
	      output.flush();

	    } catch (IOException e) {
	      e.printStackTrace();
	    }
	  }

	  static public void saveStrings(File file, String strings[]) {
	    try {
	      String location = file.getAbsolutePath();
	      createPath(location);
	      OutputStream output = new FileOutputStream(location);
	      if (file.getName().toLowerCase().endsWith(".gz")) {
	        output = new GZIPOutputStream(output);
	      }
	      saveStrings(output, strings);
	      output.close();

	    } catch (IOException e) {
	      e.printStackTrace();
	    }
	  }


	  static public void saveStrings(OutputStream output, String strings[]) {
	    try {
	      OutputStreamWriter osw = new OutputStreamWriter(output, "UTF-8");
	      PrintWriter writer = new PrintWriter(osw);
	      for (int i = 0; i < strings.length; i++) {
	        writer.println(strings[i]);
	      }
	      writer.flush();
	    } catch (UnsupportedEncodingException e) { }  // will not happen
	  }


	  /**
	   * Takes a path and creates any in-between folders if they don't
	   * already exist. Useful when trying to save to a subfolder that
	   * may not actually exist.
	   */
	  static public void createPath(String filename) {
	    File file = new File(filename);
	    String parent = file.getParent();
	    if (parent != null) {
	      File unit = new File(parent);
	      if (!unit.exists()) unit.mkdirs();
	    }
	  }



	  //////////////////////////////////////////////////////////////

	  // SORT


	  static public byte[] sort(byte what[]) {
	    return sort(what, what.length);
	  }


	  static public byte[] sort(byte[] what, int count) {
	    byte[] outgoing = new byte[what.length];
	    System.arraycopy(what, 0, outgoing, 0, what.length);
	    Arrays.sort(outgoing, 0, count);
	    return outgoing;
	  }


	  static public char[] sort(char what[]) {
	    return sort(what, what.length);
	  }


	  static public char[] sort(char[] what, int count) {
	    char[] outgoing = new char[what.length];
	    System.arraycopy(what, 0, outgoing, 0, what.length);
	    Arrays.sort(outgoing, 0, count);
	    return outgoing;
	  }


	  static public int[] sort(int what[]) {
	    return sort(what, what.length);
	  }


	  static public int[] sort(int[] what, int count) {
	    int[] outgoing = new int[what.length];
	    System.arraycopy(what, 0, outgoing, 0, what.length);
	    Arrays.sort(outgoing, 0, count);
	    return outgoing;
	  }


	  static public float[] sort(float what[]) {
	    return sort(what, what.length);
	  }


	  static public float[] sort(float[] what, int count) {
	    float[] outgoing = new float[what.length];
	    System.arraycopy(what, 0, outgoing, 0, what.length);
	    Arrays.sort(outgoing, 0, count);
	    return outgoing;
	  }


	  static public String[] sort(String what[]) {
	    return sort(what, what.length);
	  }


	  static public String[] sort(String[] what, int count) {
	    String[] outgoing = new String[what.length];
	    System.arraycopy(what, 0, outgoing, 0, what.length);
	    Arrays.sort(outgoing, 0, count);
	    return outgoing;
	  }



	  //////////////////////////////////////////////////////////////

	  // ARRAY UTILITIES


	  /**
	   * Calls System.arraycopy(), included here so that we can
	   * avoid people needing to learn about the System object
	   * before they can just copy an array.
	   */
	  static public void arraycopy(Object src, int srcPosition,
	                               Object dst, int dstPosition,
	                               int length) {
	    System.arraycopy(src, srcPosition, dst, dstPosition, length);
	  }


	  /**
	   * Convenience method for arraycopy().
	   * Identical to <CODE>arraycopy(src, 0, dst, 0, length);</CODE>
	   */
	  static public void arraycopy(Object src, Object dst, int length) {
	    System.arraycopy(src, 0, dst, 0, length);
	  }


	  /**
	   * Shortcut to copy the entire contents of
	   * the source into the destination array.
	   * Identical to <CODE>arraycopy(src, 0, dst, 0, src.length);</CODE>
	   */
	  static public void arraycopy(Object src, Object dst) {
	    System.arraycopy(src, 0, dst, 0, Array.getLength(src));
	  }

	  //

	  static public boolean[] expand(boolean list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public boolean[] expand(boolean list[], int newSize) {
	    boolean temp[] = new boolean[newSize];
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public byte[] expand(byte list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public byte[] expand(byte list[], int newSize) {
	    byte temp[] = new byte[newSize];
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public char[] expand(char list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public char[] expand(char list[], int newSize) {
	    char temp[] = new char[newSize];
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public int[] expand(int list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public int[] expand(int list[], int newSize) {
	    int temp[] = new int[newSize];
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public float[] expand(float list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public float[] expand(float list[], int newSize) {
	    float temp[] = new float[newSize];
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public String[] expand(String list[]) {
	    return expand(list, list.length << 1);
	  }

	  static public String[] expand(String list[], int newSize) {
	    String temp[] = new String[newSize];
	    // in case the new size is smaller than list.length
	    System.arraycopy(list, 0, temp, 0, Math.min(newSize, list.length));
	    return temp;
	  }


	  static public Object expand(Object array) {
	    return expand(array, Array.getLength(array) << 1);
	  }

	  static public Object expand(Object list, int newSize) {
	    Class type = list.getClass().getComponentType();
	    Object temp = Array.newInstance(type, newSize);
	    System.arraycopy(list, 0, temp, 0,
	                     Math.min(Array.getLength(list), newSize));
	    return temp;
	  }

	  //

	  // contract() has been removed in revision 0124, use subset() instead.
	  // (expand() is also functionally equivalent)

	  //

	  static public byte[] append(byte b[], byte value) {
	    b = expand(b, b.length + 1);
	    b[b.length-1] = value;
	    return b;
	  }

	  static public char[] append(char b[], char value) {
	    b = expand(b, b.length + 1);
	    b[b.length-1] = value;
	    return b;
	  }

	  static public int[] append(int b[], int value) {
	    b = expand(b, b.length + 1);
	    b[b.length-1] = value;
	    return b;
	  }

	  static public float[] append(float b[], float value) {
	    b = expand(b, b.length + 1);
	    b[b.length-1] = value;
	    return b;
	  }

	  static public String[] append(String b[], String value) {
	    b = expand(b, b.length + 1);
	    b[b.length-1] = value;
	    return b;
	  }

	  static public Object append(Object b, Object value) {
	    int length = Array.getLength(b);
	    b = expand(b, length + 1);
	    Array.set(b, length, value);
	    return b;
	  }

	  //

	  static public boolean[] shorten(boolean list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public byte[] shorten(byte list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public char[] shorten(char list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public int[] shorten(int list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public float[] shorten(float list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public String[] shorten(String list[]) {
	    return subset(list, 0, list.length-1);
	  }

	  static public Object shorten(Object list) {
	    int length = Array.getLength(list);
	    return subset(list, 0, length - 1);
	  }

	  //

	  static final public boolean[] splice(boolean list[],
	                                       boolean v, int index) {
	    boolean outgoing[] = new boolean[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public boolean[] splice(boolean list[],
	                                       boolean v[], int index) {
	    boolean outgoing[] = new boolean[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public byte[] splice(byte list[],
	                                    byte v, int index) {
	    byte outgoing[] = new byte[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public byte[] splice(byte list[],
	                                    byte v[], int index) {
	    byte outgoing[] = new byte[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public char[] splice(char list[],
	                                    char v, int index) {
	    char outgoing[] = new char[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public char[] splice(char list[],
	                                    char v[], int index) {
	    char outgoing[] = new char[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public int[] splice(int list[],
	                                   int v, int index) {
	    int outgoing[] = new int[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public int[] splice(int list[],
	                                   int v[], int index) {
	    int outgoing[] = new int[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public float[] splice(float list[],
	                                     float v, int index) {
	    float outgoing[] = new float[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public float[] splice(float list[],
	                                     float v[], int index) {
	    float outgoing[] = new float[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public String[] splice(String list[],
	                                      String v, int index) {
	    String outgoing[] = new String[list.length + 1];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    outgoing[index] = v;
	    System.arraycopy(list, index, outgoing, index + 1,
	                     list.length - index);
	    return outgoing;
	  }

	  static final public String[] splice(String list[],
	                                      String v[], int index) {
	    String outgoing[] = new String[list.length + v.length];
	    System.arraycopy(list, 0, outgoing, 0, index);
	    System.arraycopy(v, 0, outgoing, index, v.length);
	    System.arraycopy(list, index, outgoing, index + v.length,
	                     list.length - index);
	    return outgoing;
	  }


	  static final public Object splice(Object list, Object v, int index) {
	    Object[] outgoing = null;
	    int length = Array.getLength(list);

	    // check whether item being spliced in is an array
	    if (v.getClass().getName().charAt(0) == '[') {
	      int vlength = Array.getLength(v);
	      outgoing = new Object[length + vlength];
	      System.arraycopy(list, 0, outgoing, 0, index);
	      System.arraycopy(v, 0, outgoing, index, vlength);
	      System.arraycopy(list, index, outgoing, index + vlength, length - index);

	    } else {
	      outgoing = new Object[length + 1];
	      System.arraycopy(list, 0, outgoing, 0, index);
	      Array.set(outgoing, index, v);
	      System.arraycopy(list, index, outgoing, index + 1, length - index);
	    }
	    return outgoing;
	  }

	  //

	  static public boolean[] subset(boolean list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public boolean[] subset(boolean list[], int start, int count) {
	    boolean output[] = new boolean[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public byte[] subset(byte list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public byte[] subset(byte list[], int start, int count) {
	    byte output[] = new byte[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public char[] subset(char list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public char[] subset(char list[], int start, int count) {
	    char output[] = new char[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public int[] subset(int list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public int[] subset(int list[], int start, int count) {
	    int output[] = new int[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public float[] subset(float list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public float[] subset(float list[], int start, int count) {
	    float output[] = new float[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public String[] subset(String list[], int start) {
	    return subset(list, start, list.length - start);
	  }

	  static public String[] subset(String list[], int start, int count) {
	    String output[] = new String[count];
	    System.arraycopy(list, start, output, 0, count);
	    return output;
	  }


	  static public Object subset(Object list, int start) {
	    int length = Array.getLength(list);
	    return subset(list, start, length - start);
	  }

	  static public Object subset(Object list, int start, int count) {
	    Class type = list.getClass().getComponentType();
	    Object outgoing = Array.newInstance(type, count);
	    System.arraycopy(list, start, outgoing, 0, count);
	    return outgoing;
	  }

	  //

	  static public boolean[] concat(boolean a[], boolean b[]) {
	    boolean c[] = new boolean[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public byte[] concat(byte a[], byte b[]) {
	    byte c[] = new byte[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public char[] concat(char a[], char b[]) {
	    char c[] = new char[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public int[] concat(int a[], int b[]) {
	    int c[] = new int[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public float[] concat(float a[], float b[]) {
	    float c[] = new float[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public String[] concat(String a[], String b[]) {
	    String c[] = new String[a.length + b.length];
	    System.arraycopy(a, 0, c, 0, a.length);
	    System.arraycopy(b, 0, c, a.length, b.length);
	    return c;
	  }

	  static public Object concat(Object a, Object b) {
	    Class type = a.getClass().getComponentType();
	    int alength = Array.getLength(a);
	    int blength = Array.getLength(b);
	    Object outgoing = Array.newInstance(type, alength + blength);
	    System.arraycopy(a, 0, outgoing, 0, alength);
	    System.arraycopy(b, 0, outgoing, alength, blength);
	    return outgoing;
	  }

	  //

	  static public boolean[] reverse(boolean list[]) {
	    boolean outgoing[] = new boolean[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public byte[] reverse(byte list[]) {
	    byte outgoing[] = new byte[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public char[] reverse(char list[]) {
	    char outgoing[] = new char[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public int[] reverse(int list[]) {
	    int outgoing[] = new int[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public float[] reverse(float list[]) {
	    float outgoing[] = new float[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public String[] reverse(String list[]) {
	    String outgoing[] = new String[list.length];
	    int length1 = list.length - 1;
	    for (int i = 0; i < list.length; i++) {
	      outgoing[i] = list[length1 - i];
	    }
	    return outgoing;
	  }

	  static public Object reverse(Object list) {
	    Class type = list.getClass().getComponentType();
	    int length = Array.getLength(list);
	    Object outgoing = Array.newInstance(type, length);
	    for (int i = 0; i < length; i++) {
	      Array.set(outgoing, i, Array.get(list, (length - 1) - i));
	    }
	    return outgoing;
	  }



	  //////////////////////////////////////////////////////////////

	  // STRINGS


	  /**
	   * Remove whitespace characters from the beginning and ending
	   * of a String. Works like String.trim() but includes the
	   * unicode nbsp character as well.
	   */
	  static public String trim(String str) {
	    return str.replace('\u00A0', ' ').trim();
	  }


	  /**
	   * Trim the whitespace from a String array. This returns a new
	   * array and does not affect the passed-in array.
	   */
	  static public String[] trim(String[] array) {
	    String[] outgoing = new String[array.length];
	    for (int i = 0; i < array.length; i++) {
	      outgoing[i] = array[i].replace('\u00A0', ' ').trim();
	    }
	    return outgoing;
	  }


	  /**
	   * Join an array of Strings together as a single String,
	   * separated by the whatever's passed in for the separator.
	   */
	  static public String join(String str[], char separator) {
	    return join(str, String.valueOf(separator));
	  }


	  /**
	   * Join an array of Strings together as a single String,
	   * separated by the whatever's passed in for the separator.
	   * <P>
	   * To use this on numbers, first pass the array to nf() or nfs()
	   * to get a list of String objects, then use join on that.
	   * <PRE>
	   * e.g. String stuff[] = { "apple", "bear", "cat" };
	   *      String list = join(stuff, ", ");
	   *      // list is now "apple, bear, cat"</PRE>
	   */
	  static public String join(String str[], String separator) {
	    StringBuffer buffer = new StringBuffer();
	    for (int i = 0; i < str.length; i++) {
	      if (i != 0) buffer.append(separator);
	      buffer.append(str[i]);
	    }
	    return buffer.toString();
	  }


	  /**
	   * Split the provided String at wherever whitespace occurs.
	   * Multiple whitespace (extra spaces or tabs or whatever)
	   * between items will count as a single break.
	   * <P>
	   * The whitespace characters are "\t\n\r\f", which are the defaults
	   * for java.util.StringTokenizer, plus the unicode non-breaking space
	   * character, which is found commonly on files created by or used
	   * in conjunction with Mac OS X (character 160, or 0x00A0 in hex).
	   * <PRE>
	   * i.e. splitTokens("a b") -> { "a", "b" }
	   *      splitTokens("a    b") -> { "a", "b" }
	   *      splitTokens("a\tb") -> { "a", "b" }
	   *      splitTokens("a \t  b  ") -> { "a", "b" }</PRE>
	   */
	  static public String[] splitTokens(String what) {
	    return splitTokens(what, WHITESPACE);
	  }


	  /**
	   * Splits a string into pieces, using any of the chars in the
	   * String 'delim' as separator characters. For instance,
	   * in addition to white space, you might want to treat commas
	   * as a separator. The delimeter characters won't appear in
	   * the returned String array.
	   * <PRE>
	   * i.e. splitTokens("a, b", " ,") -> { "a", "b" }
	   * </PRE>
	   * To include all the whitespace possibilities, use the variable
	   * WHITESPACE, found in PConstants:
	   * <PRE>
	   * i.e. splitTokens("a   | b", WHITESPACE + "|");  ->  { "a", "b" }</PRE>
	   */
	  static public String[] splitTokens(String what, String delim) {
	    StringTokenizer toker = new StringTokenizer(what, delim);
	    String pieces[] = new String[toker.countTokens()];

	    int index = 0;
	    while (toker.hasMoreTokens()) {
	      pieces[index++] = toker.nextToken();
	    }
	    return pieces;
	  }


	  /**
	   * Split a string into pieces along a specific character.
	   * Most commonly used to break up a String along a space or a tab
	   * character.
	   * <P>
	   * This operates differently than the others, where the
	   * single delimeter is the only breaking point, and consecutive
	   * delimeters will produce an empty string (""). This way,
	   * one can split on tab characters, but maintain the column
	   * alignments (of say an excel file) where there are empty columns.
	   */
	  static public String[] split(String what, char delim) {
	    // do this so that the exception occurs inside the user's
	    // program, rather than appearing to be a bug inside split()
	    if (what == null) return null;
	    //return split(what, String.valueOf(delim));  // huh

	    char chars[] = what.toCharArray();
	    int splitCount = 0; //1;
	    for (int i = 0; i < chars.length; i++) {
	      if (chars[i] == delim) splitCount++;
	    }
	    // make sure that there is something in the input string
	    //if (chars.length > 0) {
	      // if the last char is a delimeter, get rid of it..
	      //if (chars[chars.length-1] == delim) splitCount--;
	      // on second thought, i don't agree with this, will disable
	    //}
	    if (splitCount == 0) {
	      String splits[] = new String[1];
	      splits[0] = new String(what);
	      return splits;
	    }
	    //int pieceCount = splitCount + 1;
	    String splits[] = new String[splitCount + 1];
	    int splitIndex = 0;
	    int startIndex = 0;
	    for (int i = 0; i < chars.length; i++) {
	      if (chars[i] == delim) {
	        splits[splitIndex++] =
	          new String(chars, startIndex, i-startIndex);
	        startIndex = i + 1;
	      }
	    }
	    //if (startIndex != chars.length) {
	      splits[splitIndex] =
	        new String(chars, startIndex, chars.length-startIndex);
	    //}
	    return splits;
	  }


	  /**
	   * Split a String on a specific delimiter.
	   */
	  static public String[] split(String what, String delim) {
	    return what.split(delim);
	  }


	  /**
	   * Match a string with a regular expression, and return matching groups as
	   * an array. If the sequence matches, but there are no groups, a zero length
	   * (non-null) String array will be returned. Groups are normally 1-indexed
	   * and group 0 is the matching sequence, but in this function the groups
	   * are 0-indexed. If you want matching sequence, just use the Java String
	   * methods for testing matches.
	   * @param what
	   * @param regexp
	   * @return
	   */
	  static public String[] match(String what, String regexp) {
	    Pattern p = Pattern.compile(regexp);
	    Matcher m = p.matcher(what);
	    if (m.find()) {
	      int count = m.groupCount();
	      String[] groups = new String[count];
	      for (int i = 0; i < count; i++) {
	        groups[i] = m.group(i+1);
	      }
	      return groups;
	    }
	    return null;
	  }



	  //////////////////////////////////////////////////////////////

	  // CASTING FUNCTIONS, INSERTED BY PREPROC


	  /**
	   * Convert a char to a boolean. 'T', 't', and '1' will become the
	   * boolean value true, while 'F', 'f', or '0' will become false.
	   */
	  /*
	  static final public boolean parseBoolean(char what) {
	    return ((what == 't') || (what == 'T') || (what == '1'));
	  }
	  */

	  /**
	   * <p>Convert an integer to a boolean. Because of how Java handles upgrading
	   * numbers, this will also cover byte and char (as they will upgrade to
	   * an int without any sort of explicit cast).</p>
	   * <p>The preprocessor will convert boolean(what) to parseBoolean(what).</p>
	   * @return false if 0, true if any other number
	   */
	  static final public boolean parseBoolean(int what) {
	    return (what != 0);
	  }

	  /*
	  // removed because this makes no useful sense
	  static final public boolean parseBoolean(float what) {
	    return (what != 0);
	  }
	  */

	  /**
	   * Convert the string "true" or "false" to a boolean.
	   * @return true if 'what' is "true" or "TRUE", false otherwise
	   */
	  static final public boolean parseBoolean(String what) {
	    return new Boolean(what).booleanValue();
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  /*
	  // removed, no need to introduce strange syntax from other languages
	  static final public boolean[] parseBoolean(char what[]) {
	    boolean outgoing[] = new boolean[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] =
	        ((what[i] == 't') || (what[i] == 'T') || (what[i] == '1'));
	    }
	    return outgoing;
	  }
	  */

	  /**
	   * Convert a byte array to a boolean array. Each element will be
	   * evaluated identical to the integer case, where a byte equal
	   * to zero will return false, and any other value will return true.
	   * @return array of boolean elements
	   */
	  static final public boolean[] parseBoolean(byte what[]) {
	    boolean outgoing[] = new boolean[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (what[i] != 0);
	    }
	    return outgoing;
	  }

	  /**
	   * Convert an int array to a boolean array. An int equal
	   * to zero will return false, and any other value will return true.
	   * @return array of boolean elements
	   */
	  static final public boolean[] parseBoolean(int what[]) {
	    boolean outgoing[] = new boolean[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (what[i] != 0);
	    }
	    return outgoing;
	  }

	  /*
	  // removed, not necessary... if necessary, convert to int array first
	  static final public boolean[] parseBoolean(float what[]) {
	    boolean outgoing[] = new boolean[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (what[i] != 0);
	    }
	    return outgoing;
	  }
	  */

	  static final public boolean[] parseBoolean(String what[]) {
	    boolean outgoing[] = new boolean[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = new Boolean(what[i]).booleanValue();
	    }
	    return outgoing;
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public byte parseByte(boolean what) {
	    return what ? (byte)1 : 0;
	  }

	  static final public byte parseByte(char what) {
	    return (byte) what;
	  }

	  static final public byte parseByte(int what) {
	    return (byte) what;
	  }

	  static final public byte parseByte(float what) {
	    return (byte) what;
	  }

	  /*
	  // nixed, no precedent
	  static final public byte[] parseByte(String what) {  // note: array[]
	    return what.getBytes();
	  }
	  */

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public byte[] parseByte(boolean what[]) {
	    byte outgoing[] = new byte[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = what[i] ? (byte)1 : 0;
	    }
	    return outgoing;
	  }

	  static final public byte[] parseByte(char what[]) {
	    byte outgoing[] = new byte[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (byte) what[i];
	    }
	    return outgoing;
	  }

	  static final public byte[] parseByte(int what[]) {
	    byte outgoing[] = new byte[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (byte) what[i];
	    }
	    return outgoing;
	  }

	  static final public byte[] parseByte(float what[]) {
	    byte outgoing[] = new byte[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (byte) what[i];
	    }
	    return outgoing;
	  }

	  /*
	  static final public byte[][] parseByte(String what[]) {  // note: array[][]
	    byte outgoing[][] = new byte[what.length][];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = what[i].getBytes();
	    }
	    return outgoing;
	  }
	  */

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  /*
	  static final public char parseChar(boolean what) {  // 0/1 or T/F ?
	    return what ? 't' : 'f';
	  }
	  */

	  static final public char parseChar(byte what) {
	    return (char) (what & 0xff);
	  }

	  static final public char parseChar(int what) {
	    return (char) what;
	  }

	  /*
	  static final public char parseChar(float what) {  // nonsensical
	    return (char) what;
	  }

	  static final public char[] parseChar(String what) {  // note: array[]
	    return what.toCharArray();
	  }
	  */

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  /*
	  static final public char[] parseChar(boolean what[]) {  // 0/1 or T/F ?
	    char outgoing[] = new char[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = what[i] ? 't' : 'f';
	    }
	    return outgoing;
	  }
	  */

	  static final public char[] parseChar(byte what[]) {
	    char outgoing[] = new char[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (char) (what[i] & 0xff);
	    }
	    return outgoing;
	  }

	  static final public char[] parseChar(int what[]) {
	    char outgoing[] = new char[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (char) what[i];
	    }
	    return outgoing;
	  }

	  /*
	  static final public char[] parseChar(float what[]) {  // nonsensical
	    char outgoing[] = new char[what.length];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = (char) what[i];
	    }
	    return outgoing;
	  }

	  static final public char[][] parseChar(String what[]) {  // note: array[][]
	    char outgoing[][] = new char[what.length][];
	    for (int i = 0; i < what.length; i++) {
	      outgoing[i] = what[i].toCharArray();
	    }
	    return outgoing;
	  }
	  */

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public int parseInt(boolean what) {
	    return what ? 1 : 0;
	  }

	  /**
	   * Note that parseInt() will un-sign a signed byte value.
	   */
	  static final public int parseInt(byte what) {
	    return what & 0xff;
	  }

	  /**
	   * Note that parseInt('5') is unlike String in the sense that it
	   * won't return 5, but the ascii value. This is because ((int) someChar)
	   * returns the ascii value, and parseInt() is just longhand for the cast.
	   */
	  static final public int parseInt(char what) {
	    return what;
	  }

	  /**
	   * Same as floor(), or an (int) cast.
	   */
	  static final public int parseInt(float what) {
	    return (int) what;
	  }

	  /**
	   * Parse a String into an int value. Returns 0 if the value is bad.
	   */
	  static final public int parseInt(String what) {
	    return parseInt(what, 0);
	  }

	  /**
	   * Parse a String to an int, and provide an alternate value that
	   * should be used when the number is invalid.
	   */
	  static final public int parseInt(String what, int otherwise) {
	    try {
	      int offset = what.indexOf('.');
	      if (offset == -1) {
	        return Integer.parseInt(what);
	      } else {
	        return Integer.parseInt(what.substring(0, offset));
	      }
	    } catch (NumberFormatException e) { }
	    return otherwise;
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public int[] parseInt(boolean what[]) {
	    int list[] = new int[what.length];
	    for (int i = 0; i < what.length; i++) {
	      list[i] = what[i] ? 1 : 0;
	    }
	    return list;
	  }

	  static final public int[] parseInt(byte what[]) {  // note this unsigns
	    int list[] = new int[what.length];
	    for (int i = 0; i < what.length; i++) {
	      list[i] = (what[i] & 0xff);
	    }
	    return list;
	  }

	  static final public int[] parseInt(char what[]) {
	    int list[] = new int[what.length];
	    for (int i = 0; i < what.length; i++) {
	      list[i] = what[i];
	    }
	    return list;
	  }

	  static public int[] parseInt(float what[]) {
	    int inties[] = new int[what.length];
	    for (int i = 0; i < what.length; i++) {
	      inties[i] = (int)what[i];
	    }
	    return inties;
	  }

	  /**
	   * Make an array of int elements from an array of String objects.
	   * If the String can't be parsed as a number, it will be set to zero.
	   *
	   * String s[] = { "1", "300", "44" };
	   * int numbers[] = parseInt(s);
	   *
	   * numbers will contain { 1, 300, 44 }
	   */
	  static public int[] parseInt(String what[]) {
	    return parseInt(what, 0);
	  }

	  /**
	   * Make an array of int elements from an array of String objects.
	   * If the String can't be parsed as a number, its entry in the
	   * array will be set to the value of the "missing" parameter.
	   *
	   * String s[] = { "1", "300", "apple", "44" };
	   * int numbers[] = parseInt(s, 9999);
	   *
	   * numbers will contain { 1, 300, 9999, 44 }
	   */
	  static public int[] parseInt(String what[], int missing) {
	    int output[] = new int[what.length];
	    for (int i = 0; i < what.length; i++) {
	      try {
	        output[i] = Integer.parseInt(what[i]);
	      } catch (NumberFormatException e) {
	        output[i] = missing;
	      }
	    }
	    return output;
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  /*
	  static final public float parseFloat(boolean what) {
	    return what ? 1 : 0;
	  }
	  */

	  /**
	   * Convert an int to a float value. Also handles bytes because of
	   * Java's rules for upgrading values.
	   */
	  static final public float parseFloat(int what) {  // also handles byte
	    return (float)what;
	  }

	  static final public float parseFloat(String what) {
	    return parseFloat(what, Float.NaN);
	  }

	  static final public float parseFloat(String what, float otherwise) {
	    try {
	      return new Float(what).floatValue();
	    } catch (NumberFormatException e) { }

	    return otherwise;
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  /*
	  static final public float[] parseFloat(boolean what[]) {
	    float floaties[] = new float[what.length];
	    for (int i = 0; i < what.length; i++) {
	      floaties[i] = what[i] ? 1 : 0;
	    }
	    return floaties;
	  }

	  static final public float[] parseFloat(char what[]) {
	    float floaties[] = new float[what.length];
	    for (int i = 0; i < what.length; i++) {
	      floaties[i] = (char) what[i];
	    }
	    return floaties;
	  }
	  */

	  static final public float[] parseByte(byte what[]) {
	    float floaties[] = new float[what.length];
	    for (int i = 0; i < what.length; i++) {
	      floaties[i] = what[i];
	    }
	    return floaties;
	  }

	  static final public float[] parseFloat(int what[]) {
	    float floaties[] = new float[what.length];
	    for (int i = 0; i < what.length; i++) {
	      floaties[i] = what[i];
	    }
	    return floaties;
	  }

	  static final public float[] parseFloat(String what[]) {
	    return parseFloat(what, 0);
	  }

	  static final public float[] parseFloat(String what[], float missing) {
	    float output[] = new float[what.length];
	    for (int i = 0; i < what.length; i++) {
	      try {
	        output[i] = new Float(what[i]).floatValue();
	      } catch (NumberFormatException e) {
	        output[i] = missing;
	      }
	    }
	    return output;
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public String str(boolean x) {
	    return String.valueOf(x);
	  }

	  static final public String str(byte x) {
	    return String.valueOf(x);
	  }

	  static final public String str(char x) {
	    return String.valueOf(x);
	  }

	  static final public String str(int x) {
	    return String.valueOf(x);
	  }

	  static final public String str(float x) {
	    return String.valueOf(x);
	  }

	  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

	  static final public String[] str(boolean x[]) {
	    String s[] = new String[x.length];
	    for (int i = 0; i < x.length; i++) s[i] = String.valueOf(x[i]);
	    return s;
	  }

	  static final public String[] str(byte x[]) {
	    String s[] = new String[x.length];
	    for (int i = 0; i < x.length; i++) s[i] = String.valueOf(x[i]);
	    return s;
	  }

	  static final public String[] str(char x[]) {
	    String s[] = new String[x.length];
	    for (int i = 0; i < x.length; i++) s[i] = String.valueOf(x[i]);
	    return s;
	  }

	  static final public String[] str(int x[]) {
	    String s[] = new String[x.length];
	    for (int i = 0; i < x.length; i++) s[i] = String.valueOf(x[i]);
	    return s;
	  }

	  static final public String[] str(float x[]) {
	    String s[] = new String[x.length];
	    for (int i = 0; i < x.length; i++) s[i] = String.valueOf(x[i]);
	    return s;
	  }


	  //////////////////////////////////////////////////////////////

	  // INT NUMBER FORMATTING


	  /**
	   * Integer number formatter.
	   */
	  static private NumberFormat int_nf;
	  static private int int_nf_digits;
	  static private boolean int_nf_commas;


	  static public String[] nf(int num[], int digits) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nf(num[i], digits);
	    }
	    return formatted;
	  }


	  static public String nf(int num, int digits) {
	    if ((int_nf != null) &&
	        (int_nf_digits == digits) &&
	        !int_nf_commas) {
	      return int_nf.format(num);
	    }

	    int_nf = NumberFormat.getInstance();
	    int_nf.setGroupingUsed(false); // no commas
	    int_nf_commas = false;
	    int_nf.setMinimumIntegerDigits(digits);
	    int_nf_digits = digits;
	    return int_nf.format(num);
	  }


	  static public String[] nfc(int num[]) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfc(num[i]);
	    }
	    return formatted;
	  }


	  static public String nfc(int num) {
	    if ((int_nf != null) &&
	        (int_nf_digits == 0) &&
	        int_nf_commas) {
	      return int_nf.format(num);
	    }

	    int_nf = NumberFormat.getInstance();
	    int_nf.setGroupingUsed(true);
	    int_nf_commas = true;
	    int_nf.setMinimumIntegerDigits(0);
	    int_nf_digits = 0;
	    return int_nf.format(num);
	  }


	  /**
	   * number format signed (or space)
	   * Formats a number but leaves a blank space in the front
	   * when it's positive so that it can be properly aligned with
	   * numbers that have a negative sign in front of them.
	   */
	  static public String nfs(int num, int digits) {
	    return (num < 0) ? nf(num, digits) : (' ' + nf(num, digits));
	  }

	  static public String[] nfs(int num[], int digits) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfs(num[i], digits);
	    }
	    return formatted;
	  }

	  //

	  /**
	   * number format positive (or plus)
	   * Formats a number, always placing a - or + sign
	   * in the front when it's negative or positive.
	   */
	  static public String nfp(int num, int digits) {
	    return (num < 0) ? nf(num, digits) : ('+' + nf(num, digits));
	  }

	  static public String[] nfp(int num[], int digits) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfp(num[i], digits);
	    }
	    return formatted;
	  }



	  //////////////////////////////////////////////////////////////

	  // FLOAT NUMBER FORMATTING


	  static private NumberFormat float_nf;
	  static private int float_nf_left, float_nf_right;
	  static private boolean float_nf_commas;


	  static public String[] nf(float num[], int left, int right) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nf(num[i], left, right);
	    }
	    return formatted;
	  }


	  static public String nf(float num, int left, int right) {
	    if ((float_nf != null) &&
	        (float_nf_left == left) &&
	        (float_nf_right == right) &&
	        !float_nf_commas) {
	      return float_nf.format(num);
	    }

	    float_nf = NumberFormat.getInstance();
	    float_nf.setGroupingUsed(false);
	    float_nf_commas = false;

	    if (left != 0) float_nf.setMinimumIntegerDigits(left);
	    if (right != 0) {
	      float_nf.setMinimumFractionDigits(right);
	      float_nf.setMaximumFractionDigits(right);
	    }
	    float_nf_left = left;
	    float_nf_right = right;
	    return float_nf.format(num);
	  }


	  static public String[] nfc(float num[], int right) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfc(num[i], right);
	    }
	    return formatted;
	  }


	  static public String nfc(float num, int right) {
	    if ((float_nf != null) &&
	        (float_nf_left == 0) &&
	        (float_nf_right == right) &&
	        float_nf_commas) {
	      return float_nf.format(num);
	    }

	    float_nf = NumberFormat.getInstance();
	    float_nf.setGroupingUsed(true);
	    float_nf_commas = true;

	    if (right != 0) {
	      float_nf.setMinimumFractionDigits(right);
	      float_nf.setMaximumFractionDigits(right);
	    }
	    float_nf_left = 0;
	    float_nf_right = right;
	    return float_nf.format(num);
	  }


	  /**
	   * Number formatter that takes into account whether the number
	   * has a sign (positive, negative, etc) in front of it.
	   */
	  static public String[] nfs(float num[], int left, int right) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfs(num[i], left, right);
	    }
	    return formatted;
	  }

	  static public String nfs(float num, int left, int right) {
	    return (num < 0) ? nf(num, left, right) :  (' ' + nf(num, left, right));
	  }


	  static public String[] nfp(float num[], int left, int right) {
	    String formatted[] = new String[num.length];
	    for (int i = 0; i < formatted.length; i++) {
	      formatted[i] = nfp(num[i], left, right);
	    }
	    return formatted;
	  }

	  static public String nfp(float num, int left, int right) {
	    return (num < 0) ? nf(num, left, right) :  ('+' + nf(num, left, right));
	  }



	  //////////////////////////////////////////////////////////////

	  // HEX/BINARY CONVERSION


	  static final public String hex(byte what) {
	    return hex(what, 2);
	  }

	  static final public String hex(char what) {
	    return hex(what, 4);
	  }

	  static final public String hex(int what) {
	    return hex(what, 8);
	  }

	  static final public String hex(int what, int digits) {
	    String stuff = Integer.toHexString(what).toUpperCase();

	    int length = stuff.length();
	    if (length > digits) {
	      return stuff.substring(length - digits);

	    } else if (length < digits) {
	      return "00000000".substring(8 - (digits-length)) + stuff;
	    }
	    return stuff;
	  }

	  static final public int unhex(String what) {
	    // has to parse as a Long so that it'll work for numbers bigger than 2^31
	    return (int) (Long.parseLong(what, 16));
	  }

	  //

	  /**
	   * Returns a String that contains the binary value of a byte.
	   * The returned value will always have 8 digits.
	   */
	  static final public String binary(byte what) {
	    return binary(what, 8);
	  }

	  /**
	   * Returns a String that contains the binary value of a char.
	   * The returned value will always have 16 digits because chars
	   * are two bytes long.
	   */
	  static final public String binary(char what) {
	    return binary(what, 16);
	  }

	  /**
	   * Returns a String that contains the binary value of an int.
	   * The length depends on the size of the number itself.
	   * An int can be up to 32 binary digits, but that seems like
	   * overkill for almost any situation, so this function just
	   * auto-size. If you want a specific number of digits (like all 32)
	   * use binary(int what, int digits) to specify how many digits.
	   */
	  static final public String binary(int what) {
	    return Integer.toBinaryString(what);
	    //return binary(what, 32);
	  }

	  /**
	   * Returns a String that contains the binary value of an int.
	   * The digits parameter determines how many digits will be used.
	   */
	  static final public String binary(int what, int digits) {
	    String stuff = Integer.toBinaryString(what);

	    int length = stuff.length();
	    if (length > digits) {
	      return stuff.substring(length - digits);

	    } else if (length < digits) {
	      int offset = 32 - (digits-length);
	      return "00000000000000000000000000000000".substring(offset) + stuff;
	    }
	    return stuff;
	  }


	  /**
	   * Unpack a binary String into an int.
	   * i.e. unbinary("00001000") would return 8.
	   */
	  static final public int unbinary(String what) {
	    return Integer.parseInt(what, 2);
	  }



	  static public int blendColor(int c1, int c2, int mode) {
	    return PGraphics.blendColor(c1, c2, mode);
	  }


	  static public int lerpColor(int c1, int c2, float amt, int mode) {
	    return PGraphics.lerpColor(c1, c2, amt, mode);
	  }
}
