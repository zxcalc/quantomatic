import java.io.*;

public class QuantoBack {

	final static String ml_command = "isabelle -e Controller.init(); ";

	final static String local_quanto_heap = "/.quantomatic/quanto";

	Process backEnd;
	BufferedReader from_backEnd;
	BufferedWriter to_backEnd;

	public QuantoBack() {
		try {
			String homedir = System.getenv("HOME");

			backEnd = Runtime.getRuntime().exec(
					ml_command + homedir + local_quanto_heap);
			from_backEnd = new BufferedReader(new InputStreamReader(backEnd
					.getInputStream()));
			to_backEnd = new BufferedWriter(new OutputStreamWriter(backEnd
					.getOutputStream()));
			System.out.println("Initialising QuantoML...");
			send("H\n"); // ask for back end status
			System.out.println(receive()); //  print it out
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
				System.out.println(ln);
				message.append(ln);
				ln = from_backEnd.readLine();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return message.toString();

	}
}
