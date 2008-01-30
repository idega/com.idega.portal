/*
 * Created on 30.3.2004
 */
package com.idega.portal;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.faces.context.ResponseWriter;
import javax.servlet.ServletOutputStream;

/**
 * @author tryggvil
 */
public class PortletJSFResponseStream extends ServletOutputStream {

	//protected ByteArrayOutputStream baos = null;
	protected boolean closed = false;
	private ResponseWriter responseWriter;

	public PortletJSFResponseStream(ResponseWriter responseWriter) throws IOException {
		super();
		this.closed = false;
		//this.response = response;
		this.responseWriter=responseWriter;
	}

	public void close() throws IOException {
		if (this.closed) {
			throw new IOException("This output stream has already been closed");
		}
		this.responseWriter.close();
		this.closed = true;
	}

	public void flush() throws IOException {
		if (this.closed) {
			throw new IOException("Cannot flush a closed output stream");
		}
		this.responseWriter.flush();
	}

	public void write(int b) throws IOException {
		if (this.closed) {
			throw new IOException("Cannot write to a closed output stream");
		}
		this.responseWriter.write(b);
	}

	public void write(byte b[]) throws IOException {
		write(b, 0, b.length);
	}

	public void write(byte b[], int off, int len) throws IOException {
		//System.out.println("writing...");
		if (this.closed) {
			throw new IOException("Cannot write to a closed output stream");
		}
		char[] charArr = new char[b.length];
		System.arraycopy(b, 0, charArr, 0, b.length);
		write(charArr,off,len);
		
	}
	
	public void write(char c[], int off, int len) throws IOException {
		//System.out.println("writing...");
		if (this.closed) {
			throw new IOException("Cannot write to a closed output stream");
		}
		this.responseWriter.write(c,off,len);	
	}
	
	public void write(char c[]) throws IOException {
		write(c,0,c.length);
	}
	public boolean closed() {
		return (this.closed);
	}

	public void reset() {
		//noop
	}


	private void writeln() throws IOException {
		write("\n".toCharArray());
	}
	
	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(boolean)
	 */
	@Override
	public void print(boolean arg0) throws IOException {
		// TODO Auto-generated method stub
		print(new Boolean(arg0).toString());
	}


	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(char)
	 */
	@Override
	public void print(char c) throws IOException {
		// TODO Auto-generated method stub
		write(c);
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(double)
	 */
	@Override
	public void print(double d) throws IOException {
		// TODO Auto-generated method stub
		print(new Double(d).toString());
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(float)
	 */
	@Override
	public void print(float f) throws IOException {
		// TODO Auto-generated method stub
		print(new Float(f).toString());
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(int)
	 */
	@Override
	public void print(int i) throws IOException {
		// TODO Auto-generated method stub
		write(i);
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(long)
	 */
	@Override
	public void print(long l) throws IOException {
		// TODO Auto-generated method stub
		print(new Long(l).toString());
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#print(java.lang.String)
	 */
	@Override
	public void print(String arg0) throws IOException {
		// TODO Auto-generated method stub
		write(arg0.toCharArray());
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println()
	 */
	@Override
	public void println() throws IOException {
		writeln();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(boolean)
	 */
	@Override
	public void println(boolean b) throws IOException {
		// TODO Auto-generated method stub
		print(b);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(char)
	 */
	@Override
	public void println(char c) throws IOException {
		// TODO Auto-generated method stub
		print(c);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(double)
	 */
	@Override
	public void println(double d) throws IOException {
		print(d);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(float)
	 */
	@Override
	public void println(float f) throws IOException {
		print(f);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(int)
	 */
	@Override
	public void println(int i) throws IOException {
		print(i);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(long)
	 */
	@Override
	public void println(long l) throws IOException {
		print(l);
		println();
	}

	/* (non-Javadoc)
	 * @see javax.servlet.ServletOutputStream#println(java.lang.String)
	 */
	@Override
	public void println(String s) throws IOException {
		print(s);
		println();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone() throws CloneNotSupportedException {
		// TODO Auto-generated method stub
		return super.clone();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		return super.equals(obj);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		// TODO Auto-generated method stub
		super.finalize();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		// TODO Auto-generated method stub
		return super.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}
}