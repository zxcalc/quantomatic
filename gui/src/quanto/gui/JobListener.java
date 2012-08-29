/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package quanto.gui;

import java.util.EventListener;

/**
 *
 * @author alemer
 */
public interface JobListener extends EventListener
{

	/**
	 * Notifies the listener that the job has terminated.
	 *
	 * Guaranteed to be sent exactly once in the life of a job.
	 * @param event
	 */
	void jobEnded(JobEndEvent event);
    
}
