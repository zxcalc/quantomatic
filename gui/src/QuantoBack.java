import java.io.*;
//import java.util.Map;

public class QuantoBack {

	//final static String ml_command = "isabelle";
	//final static String local_quanto_heap = "/.quantomatic/quanto";

	Process backEnd;
	BufferedReader from_backEnd;
	BufferedReader from_backEndError;
	BufferedWriter to_backEnd;

	public QuantoBack() {
		try {
			//String homedir = System.getProperty("user.home");
			//String heap = homedir + local_quanto_heap;
			ProcessBuilder pb = new ProcessBuilder("quantomatic");
			
			//Map<String,String> env = pb.environment();
			//env.put("PATH", env.get("PATH") + ":/usr/local/bin");
			backEnd = pb.start();
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			from_backEndError = new BufferedReader(new InputStreamReader(backEnd
					.getErrorStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			
			System.out.println("Initialising QuantoML...");
			System.out.println(receive());
			
			send("H\n"); // ask for back end status
			/*String ln = from_backEndError.readLine();
			while (ln != null) {
				System.out.println("ERROR: "+ln);
				ln = from_backEndError.readLine();
			}*/
			
			// Make sure we eat up any garbage output before the status.
			String rcv = receive();
			while (!rcv.contains("Hello from QUANTOMATIC")) {
				System.out.println(rcv);
				rcv = receive();
			}
			
			System.out.println("status:");
			System.out.println(rcv); //  print it out
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void send(String command) {
		try {
			to_backEnd.write(command);
			to_backEnd.newLine();
			to_backEnd.flush();
		} catch (IOException e) {
			e.printStackTrace();
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
			e.printStackTrace();
		}
		return message.toString();
	}
}
