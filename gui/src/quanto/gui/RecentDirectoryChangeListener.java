package quanto.gui;

import java.io.File;
import java.util.EventListener;

/**
 *
 * @author alex
 */
public interface RecentDirectoryChangeListener extends EventListener {
	void recentDirectoryChanged(Object source, File directory);
}
