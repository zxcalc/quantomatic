package quanto;
import java.io.*;
//import java.util.Map;

public class QuantoBack {
 
	Process backEnd;
	BufferedReader from_backEnd;
	BufferedReader from_backEndError;
	BufferedWriter to_backEnd;

	public QuantoBack() {
		try {
			//String homedir = System.getProperty("user.home");
			//String heap = homedir + local_quanto_heap;
			
			//Map<String,String> env = pb.environment();
			//env.put("PATH", env.get("PATH") + ":/usr/local/bin");
			
			ProcessBuilder pb = new ProcessBuilder("quanto-core");
			
			System.out.println("Initialising QuantoML...");
			backEnd = pb.start();
			
			System.out.println("Connecting pipes...");
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			from_backEndError = new BufferedReader(new InputStreamReader(backEnd
					.getErrorStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			
			System.out.println("Sending hello...");
			send("H\n"); // ask for back end status
			
			System.out.println("Getting hello...");
			// Make sure we eat up any output before the status.
			String rcv = receive();
			while (rcv != null && !rcv.contains("Hello from QUANTOMATIC")) {
				System.out.println("QuantoCore Sent: " + rcv);
				rcv = receive();
			}
			
			System.out.println("Status:");
			System.out.println(rcv); //  print it out
		} catch (IOException e) {
			e.printStackTrace();
			if(backEnd == null) { System.out.println("ERROR: Cannot execute: quanto-core, check it is in the path."); }
			else {System.out.println("Exit value from backend: " + backEnd.exitValue()); }
		}
	}

	public void send(String command) {
		if(to_backEnd != null){
			try {
				to_backEnd.write(command);
				to_backEnd.newLine();
				to_backEnd.flush();
			} catch (IOException e) {
				System.out.println("Exit value from backend: "
						+ backEnd.exitValue());
				e.printStackTrace();
			}
		}
	}

	public String receive() {
		StringBuffer message = new StringBuffer();
		try {
			String ln = from_backEnd.readLine();
			while (!ln.equals("stop")) {
				message.append(ln);
				message.append('\n');
				ln = from_backEnd.readLine();
			}
		} catch (IOException e) {
			System.out.println("Exit value from backend: " + backEnd.exitValue());
			e.printStackTrace();
		}
		catch (java.lang.NullPointerException e) {
			System.out.println("Exit value from backend: " + backEnd.exitValue());
			e.printStackTrace();
			return null;
		}
		return message.toString();
	}
}
