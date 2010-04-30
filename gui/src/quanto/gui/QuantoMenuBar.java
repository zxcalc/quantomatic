/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package quanto.gui;

import java.util.HashMap;
import java.util.Map;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;


/**
 *
 * @author alex
 */
public class QuantoMenuBar extends JMenuBar {
	private Map<String, JMenu> menus = new HashMap<String, JMenu>();

	public static final String FILE_MENU = "file";
	public static final String EDIT_MENU = "edit";
	public static final String VIEW_MENU = "view";
	public static final String GRAPH_MENU = "graph";
	public static final String HILBERT_MENU = "hilbert";
	public static final String RULE_MENU = "rule";

	public JMenu addMenu(String key, String name)
	{
		JMenu menu = menus.get(key);
		if (menu == null)
		{
			menu = new JMenu(name);
			menus.put(key, menu);
			add(menu);
		}
		return menu;
	}

	public JMenu addMenu(String key, String name, int mnemonic)
	{
		JMenu menu = menus.get(key);
		if (menu == null)
		{
			menu = new JMenu(name);
			menu.setMnemonic(mnemonic);
			menus.put(key, menu);
			add(menu);
		}
		return menu;
	}

	public JMenu addMenu(String key, String name, char mnemonic)
	{
		JMenu menu = menus.get(key);
		if (menu == null)
		{
			menu = new JMenu(name);
			menu.setMnemonic(mnemonic);
			menus.put(key, menu);
			add(menu);
		}
		return menu;
	}

	public JMenu getMenu(String key)
	{
		return menus.get(key);
	}

	private JMenu forceGetMenu(String key)
	{
		JMenu menu = getMenu(key);
		if (menu == null) {
			throw new IllegalArgumentException(
				"Menu " + key + " does not exist");
		}
		return menu;
	}

	public void appendToMenu(String menu, Action action)
	{
		forceGetMenu(menu).add(action);
	}

	public void prependToMenu(String menu, Action action)
	{
		forceGetMenu(menu).insert(action, 0);
	}

	public void insertInMenu(String menu, Action action, int position)
	{
		forceGetMenu(menu).insert(action, position);
	}

	public void insertBeforeInMenu(String menu, Action target, Action action)
	{
		JMenu m = forceGetMenu(menu);
		int pos = getIndexOf(menu, target);
		if (pos == -1) {
			throw new IllegalArgumentException(
				"Action '" + target.getValue(Action.NAME) +
				"' not in menu '" + menu + "'");
		}
		m.insert(action, pos);
	}

	public void insertAfterInMenu(String menu, Action target, Action action)
	{
		JMenu m = forceGetMenu(menu);
		int pos = getIndexOf(menu, target);
		if (pos == -1) {
			throw new IllegalArgumentException(
				"Action '" + target.getValue(Action.NAME) +
				"' not in menu '" + menu + "'");
		}
		m.insert(action, pos + 1);
	}

	public int getIndexOf(String menu, JMenuItem mi) {
		return getIndexOf(forceGetMenu(menu), mi);
	}

	public int getIndexOf(JMenu m, JMenuItem mi) {
		for (int i = 0; i < m.getItemCount(); i++) {
			if (m.getComponent(i).equals(mi)) {
				return i;
			}
		}
		return -1;
	}

	public int getIndexOf(String menu, Action action) {
		return getIndexOf(forceGetMenu(menu), action);
	}

	public int getIndexOf(JMenu m, Action action) {
		for (int i = 0; i < m.getItemCount(); i++) {
			JMenuItem item = m.getItem(i);
			if (item != null && action.equals(item.getAction())) {
				return i;
			}
		}
		return -1;
	}
}
