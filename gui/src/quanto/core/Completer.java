package quanto.core;

import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A class for storing and retrieving command completions.
 * @author alek
 *
 */
public class Completer {
	private SortedSet<String> lexicon;
	
	public Completer() {
		lexicon = new TreeSet<String>();
	}
	
	public void addWord(String word) {
		lexicon.add(word);
	}
	
	public SortedSet<String> getCompletions(String prefix) {
		return lexicon.subSet(prefix, prefix + Character.MAX_VALUE);
	}
	
	/**
	 * Find the greatest common prefix of a sorted set.
	 * @param compl
	 * @return
	 */
	public static String greatestCommonPrefix(SortedSet<String> compl) {
		StringBuilder buf = new StringBuilder(compl.last().length());
		char[] s1 = compl.first().toCharArray();
		char[] s2 = compl.last().toCharArray();
		for (int i=0; i<Math.min(s1.length, s2.length); ++i) {
			if (s1[i]==s2[i]) buf.append(s1[i]);
			else break;
		}
		return buf.toString();
	}

	public SortedSet<String> getLexicon() {
		return lexicon;
	}

	public void setLexicon(SortedSet<String> lexicon) {
		this.lexicon = lexicon;
	}
}
