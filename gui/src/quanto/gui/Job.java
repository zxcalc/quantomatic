/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;

/**
 * A separate thread that executes some job on the graph
 * asynchronously.
 *
 * This mainly exists to allow the job to be displayed to the user
 * and aborted.
 *
 * The job must call fireJobFinished() when it has come to a natural
 * end.  It may also call fireJobAborted() when it is interrupted,
 * but should work fine even if it doesn't.
 */
public abstract class Job extends Thread
{
	private EventListenerList listenerList = new EventListenerList();
	private JobEndEvent jobEndEvent = null;

	/**
	 * Abort the job.  The default implementation interrupts the
	 * thread and calls fireJobAborted().
	 */
	public void abortJob() {
		this.interrupt();
		fireJobAborted();
	}

	/**
	 * Add a job listener.
	 *
	 * All job listener methods execute in the context of the
	 * AWT event queue.
	 * @param l
	 */
	public void addJobListener(JobListener l) {
		listenerList.add(JobListener.class, l);
	}

	public void removeJobListener(JobListener l) {
		listenerList.remove(JobListener.class, l);
	}

	/**
	 * Notify listeners that the job has finished successfully,
	 * if no notification has already been sent.
	 */
	protected final void fireJobFinished() {
		if (jobEndEvent == null) {
			fireJobEnded(false);
		}
	}

	/**
	 * Notify listeners that the job has been aborted, if no
	 * notification has already been sent.
	 */
	protected final void fireJobAborted() {
		if (jobEndEvent == null) {
			fireJobEnded(true);
		}
	}

	private void fireJobEnded(final boolean aborted) {
		SwingUtilities.invokeLater(new Runnable() {

			public void run() {
				// Guaranteed to return a non-null array
				Object[] listeners = listenerList.getListenerList();
				// Process the listeners last to first, notifying
				// those that are interested in this event
				for (int i = listeners.length - 2; i >= 0; i -= 2) {
					if (listeners[i] == JobListener.class) {
						// Lazily create the event:
						if (jobEndEvent == null) {
							jobEndEvent = new JobEndEvent(this, aborted);
						}
						((JobListener) listeners[i + 1]).jobEnded(jobEndEvent);
					}
				}
			}
		});
	}
    
}
