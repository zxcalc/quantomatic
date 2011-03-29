package quanto.core;


public interface Rewrite<G extends CoreGraph> {
	public G getLhs();
	public G getRhs();
}
