package quanto.core.data;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TexConstants {

	static Map<String, String> constants = null;
	static Pattern latex = Pattern.compile("\\\\([A-Za-z]*)");

	private static void initialize() {
		Map<String, String> c = new HashMap<String, String>(50);

		// greek
		c.put("alpha", "\u03b1");
		c.put("beta", "\u03b2");
		c.put("gamma", "\u03b3");
		c.put("delta", "\u03b4");
		c.put("epsilon", "\u03b5");
		c.put("zeta", "\u03b6");
		c.put("eta", "\u03b7");
		c.put("theta", "\u03b8");
		c.put("iota", "\u03b9");
		c.put("kappa", "\u03ba");
		c.put("lambda", "\u03bb");
		c.put("mu", "\u03bc");
		c.put("nu", "\u03bd");
		c.put("xi", "\u03be");
		c.put("pi", "\u03c0");
		c.put("rho", "\u03c1");
		c.put("sigma", "\u03c3");
		c.put("tau", "\u03c4");
		c.put("upsilon", "\u03c5");
		c.put("phi", "\u03c6");
		c.put("chi", "\u03c7");
		c.put("psi", "\u03c8");
		c.put("omega", "\u03c9");
		c.put("Gamma", "\u0393");
		c.put("Delta", "\u0394");
		c.put("Theta", "\u0398");
		c.put("Lambda", "\u039b");
		c.put("Xi", "\u039e");
		c.put("Pi", "\u03a0");
		c.put("Sigma", "\u03a3");
		c.put("Upsilon", "\u03a5");
		c.put("Phi", "\u03a6");
		c.put("Psi", "\u03a8");
		c.put("Omega", "\u03a9");

		constants = c;
	}

	public static String translate(String input) {
		if (constants == null) {
			initialize();
		}

		Matcher m = latex.matcher(input);
		StringBuffer buf = new StringBuffer();
		while (m.find()) {
			String ucode = constants.get(m.group(1));
			if (ucode != null) {
				m.appendReplacement(buf, ucode);
			}
		}
		m.appendTail(buf);

		return buf.toString();
	}
}
