package quanto;

public class GlobalNameSpace {
	static Integer id = 0;
	
	public static Integer newName() {
		id += 1;
		return id;
	}
}
