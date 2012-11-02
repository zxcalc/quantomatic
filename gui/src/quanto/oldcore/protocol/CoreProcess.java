package quanto.oldcore.protocol;

import quanto.util.StreamRedirector;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import quanto.core.CoreException;
import quanto.core.CoreProtocolException;

/**
 * Manages an instance of the core process.
 *
 * @author alemer
 */
public class CoreProcess {

    private final static Logger logger = Logger.getLogger("quanto.core.protocol");
	
    public static String quantoCoreExecutable = "quanto-core";
	
    private Process backend;
	private CoreTalker talker = new CoreTalker();

	public CoreTalker getTalker() {
		return talker;
	}

    public void startCore() throws CoreException {
		startCore(quantoCoreExecutable);
    }

    public void startCore(String executable) throws CoreException {
        try {
            ProcessBuilder pb = new ProcessBuilder(executable, "--protocol");

            pb.redirectErrorStream(false);
            logger.log(Level.FINEST, "Starting {0}...", executable);
            backend = pb.start();
            logger.log(Level.FINEST, "{0} started successfully", executable);

            new StreamRedirector(backend.getErrorStream(), System.err).start();
        } catch (IOException e) {
            logger.log(Level.SEVERE,
                    "Could not execute \"" + executable + "\": "
                    + e.getMessage(),
                    e);
            throw new CoreProtocolException(String.format(
                    "Could not execute \"%1$\": %2$", executable,
                    e.getMessage()), e);
        }
        try {
			talker.connect(backend.getInputStream(), backend.getOutputStream());
        } catch (IOException e) {
            logger.log(Level.SEVERE,
                    "The core failed to initiate the protocol correctly",
                    e);
            throw new CoreProtocolException(
                    "The core failed to initiate the protocol correctly",
                    e);
        }

    }

    private static class ProcessCleanupThread extends Thread {

        private Process process;

        public ProcessCleanupThread(Process process) {
            super("Process cleanup thread");
            this.process = process;
        }

        @Override
        public void run() {
            try {
                logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit");
                sleep(5000);
            } catch (InterruptedException ex) {
                logger.log(Level.FINER, "Thread interupted");
            }
            logger.log(Level.FINER, "Forcibly terminating the core process");
            process.destroy();
        }
    }

    /**
     * Quits the core process, and releases associated resources
     */
    public void killCore() {
        if (backend != null) {
            logger.log(Level.FINEST, "Shutting down the core process");
			talker.disconnect();
			new ProcessCleanupThread(backend).start();
            backend = null;
        }
    }
}
