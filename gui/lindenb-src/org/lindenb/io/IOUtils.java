package org.lindenb.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.zip.GZIPInputStream;

public class IOUtils
	{
	/** @return a representation of a StreamTokenizer  */
	public static String toString(StreamTokenizer st)
	    {
		if(st==null) return "null";
	    switch(st.ttype)
	        {
	        case StreamTokenizer.TT_EOF: return "<EOF>";
	        case StreamTokenizer.TT_EOL: return "<EOL>";
	        case StreamTokenizer.TT_NUMBER: return String.valueOf(st.nval)+"(number)";
	        case StreamTokenizer.TT_WORD: return st.sval+"(word)";
	        default: return  "'"+((char)st.ttype)+"'(char)";
	        }
	    }
	
	public static String getReaderContent(Reader r) throws IOException
		{
		StringWriter w= new StringWriter();
		copyTo(r,w);
		return w.toString();
		}
	
	
	public static String getFileContent(File file) throws IOException
		{
		FileReader r=new FileReader(file);
		String s=getReaderContent(r);
		r.close();
		return s;
		}
	
	public static String getURLContent(URL url) throws IOException
		{
		InputStreamReader r=new InputStreamReader(url.openStream());
		String s=getReaderContent(r);
		r.close();
		return s;
		}
	
	public static void copyToDir(File file, File dir) throws IOException
		{
		File dest=new File(dir,file.getName());
		if(file.equals(dest)) throw new IOException("copyToDir src==dest file");
		copyTo(file,dest);
		}
	
	public static void copyTo(File src, File dest) throws IOException
		{
		if(src.equals(dest)) throw new IOException("copyTo src==dest file");
		FileOutputStream fout=new FileOutputStream(dest);
		InputStream in=new FileInputStream(src);
		IOUtils.copyTo(in, fout);
		fout.flush();
		fout.close();
		in.close();
		}
	
	
	public static void copyTo(InputStream in, OutputStream out) throws IOException
		{
		byte buffer[]=new byte[2048];
		int n=0;
		while((n=in.read(buffer))!=-1)
			{
			out.write(buffer, 0, n);
			}
		out.flush();
		}
	
	public static void copyTo(Reader in, Writer out) throws IOException
		{
		char buffer[]=new char[2048];
		int n=0;
		while((n=in.read(buffer))!=-1)
			{
			out.write(buffer, 0, n);
			}
		out.flush();
		}
	
	/**
	 * answers a BufferedReader to the given uri.
	 * if uri starts with a URL schema (http,ftp, etc...) then we can URL.openStream, else this is a file
	 * if uri ends with *.gz, a GZIPInputStream is added to decode the gzipped-stream
	 * @param uri the uri
	 * @return an input stream to the uri
	 * @throws IOException
	 */
	public static BufferedReader openReader(String uri) throws IOException
		{
		if(	uri.startsWith("http://") ||
				uri.startsWith("https://") ||
				uri.startsWith("file://") ||
				uri.startsWith("ftp://")
	    		)
	    		{
	    		InputStream in= openInputStream(uri);
	    		return new BufferedReader(new InputStreamReader(in));
	        	}
	    	else
	        	{
	        	return openFile( new File(uri));
	        	}
		}
	
	/**
	 * answers an input stream to the given uri.
	 * if uri starts with a URL schema (http,ftp, etc...) then we can URL.openStream, else this is a file
	 * if uri ends with *.gz, a GZIPInputStream is added to decode the gzipped-stream
	 * @param uri the uri
	 * @return an input stream to the uri
	 * @throws IOException
	 */
	public static InputStream openInputStream(String uri) throws IOException
		{
		InputStream in=null;
		if(	uri.startsWith("http://") ||
				uri.startsWith("https://") ||
				uri.startsWith("file://") ||
				uri.startsWith("ftp://")
	    		)
	    		{
	    		URL url= new URL(uri);
	    		in = url.openStream();
	    		}
	    	else
	        	{
	    		in=new FileInputStream(uri);
	        	}
			if(uri.toLowerCase().endsWith(".gz"))
	    		{
	    		return new GZIPInputStream(in);
	    		}
			return in;
			}
	
	
	/** open a file and return a BufferedReader, gunzip the file if it ends with *.gz*/
	public static BufferedReader openFile(File file) throws IOException
		{
		if(	file.getName().endsWith(".gz"))
			{
			return new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))));
			}
		return new BufferedReader(new FileReader(file));
		}

	/** flush a stream without throwing an exception */
	public static void safeFlush(OutputStream out)
		{
		if(out==null) return;
		try { out.flush(); } catch(IOException err) {}
		}
	/** flush a writer without throwing an  exception */
	public static void safeFlush(Writer out)
		{
		if(out==null) return;
		try { out.flush(); } catch(IOException err) {}
		}
	
	/** close a stream without throwing an exception */
	public static void safeClose(OutputStream out)
		{
		if(out==null) return;
		try { out.close(); } catch(IOException err) {}
		}
	/** close a writer without throwing an  exception */
	public static void safeClose(Writer out)
		{
		if(out==null) return;
		try { out.close(); } catch(IOException err) {}
		}
	/** close a stream without throwing an exception */
	public static void safeClose(InputStream in)
		{
		if(in==null) return;
		try { in.close(); } catch(IOException err) {}
		}
	/** close a writer without throwing an  exception */
	public static void safeClose(Reader in)
		{
		if(in==null) return;
		try { in.close(); } catch(IOException err) {}
		}
	
	/** creates a new directory in the default tmp directory*/
	public static File createTempDir() throws IOException
		{
		return createTempDir(null);
		}
	
	/** creates a new directory in the given directory*/
	public static File createTempDir(File parentDir) throws IOException
		{
		File dir= File.createTempFile("_tmp_dir_", ".dir",parentDir);
		if(!(dir.delete()))//it is a FILE, delete it and make it a directory
		    {
		    throw new IOException("Could not delete file: " + dir.getAbsolutePath());
		    }
		if(!(dir.mkdir()))
		    {
		    throw new IOException("Could not create temp directory: " + dir.getAbsolutePath());
		    }
		return dir;
		}
	
	}
