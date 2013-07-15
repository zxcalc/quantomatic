package org.lindenb.awt;

import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;

/** simple implementation of  java.awt.geom.Dimension2D */
public abstract class Dimension2D
	extends java.awt.geom.Dimension2D
	{
	public static class Double extends Dimension2D
		{
		public double width=0.0;
		public double height=0.0;
		
		/** initialize using the width and height of the RectangularShape */
		public Double(RectangularShape shape)
			{
			this(shape.getWidth(),shape.getHeight());
			}
		
		public Double(java.awt.geom.Dimension2D cp)
			{
			this(cp.getWidth(),cp.getHeight());
			}
		
		public Double(double width,double height)
			{
			this.width=width;
			this.height=height;
			}
		public Double()
			{
			this(0,0);
			}
		@Override
		public double getWidth() {
			return this.width;
			}
		@Override
		public double getHeight() {
			return this.height;
			}
		@Override
		public void setSize(double width, double height) {
			this.width=width;
			this.height=height;
			}
		@Override
		public Object clone() {
			return new Dimension2D.Double(this);
			}
		}
	
	public static class Float extends Dimension2D
		{
		public float width=0.0f;
		public float height=0.0f;
		public Float(float width,float height)
			{
			this.width=width;
			this.height=height;
			}
		
		/** initialize using the width and height of the RectangularShape */
		public Float(RectangularShape shape)
			{
			this((float)shape.getWidth(),(float)shape.getHeight());
			}
	
		public Float(java.awt.geom.Dimension2D cp)
			{
			this((float)cp.getWidth(),(float)cp.getHeight());
			}
		
		public Float()
			{
			this(0f,0f);
			}
		@Override
		public double getWidth() {
			return this.width;
			}
		@Override
		public double getHeight() {
			return this.height;
			}
		@Override
		public void setSize(double width, double height) {
			this.width=(float)width;
			this.height=(float)height;
			}
		
		@Override
		public Object clone() {
			return new Dimension2D.Float(this);
			}
		}
	/** set the maximum size of this and another shape. Useful when searching for
	 * the maximum size of an area containing some shapes
	 * @param other
	 */
	public void max(Shape shape)
		{
		Rectangle2D r= shape.getBounds2D();
		max(r.getWidth(),r.getHeight());
		}
	
	/** set the maximum size of this and another shape. Useful when searching for
	 * the maximum size of an area containing some shapes
	 * @param other
	 */
	public void max(double width,double height)
		{
		setSize(
				Math.max(this.getWidth(),width),
				Math.max(this.getHeight(),height)
				);
		}
	
	
	@Override
	public int hashCode() {
		return 	new java.lang.Double(getWidth()).hashCode()+
		 		new java.lang.Double(getHeight()).hashCode();
		}
	
	@Override
	public boolean equals(Object obj) {
		if(obj==this) return true;
		if(obj==null || !(obj instanceof java.awt.geom.Dimension2D)) return false;
		java.awt.geom.Dimension2D cp=java.awt.geom.Dimension2D.class.cast(obj);
		return getWidth()==cp.getWidth() && getHeight()==cp.getHeight();
		}
	}
