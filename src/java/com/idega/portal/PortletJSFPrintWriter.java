package com.idega.portal;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.Locale;

import javax.faces.context.ResponseWriter;

public class PortletJSFPrintWriter extends PrintWriter {

	ResponseWriter responseWriter;
	
	public PortletJSFPrintWriter(OutputStream out,ResponseWriter responseWriter) {
		super(out);
		this.responseWriter=responseWriter;
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#append(char)
	 */
	@Override
	public PrintWriter append(char c) {
		// TODO Auto-generated method stub
		return super.append(c);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#append(java.lang.CharSequence, int, int)
	 */
	@Override
	public PrintWriter append(CharSequence csq, int start, int end) {
		// TODO Auto-generated method stub
		return super.append(csq, start, end);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#append(java.lang.CharSequence)
	 */
	@Override
	public PrintWriter append(CharSequence csq) {
		// TODO Auto-generated method stub
		return super.append(csq);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#checkError()
	 */
	@Override
	public boolean checkError() {
		// TODO Auto-generated method stub
		return super.checkError();
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#close()
	 */
	@Override
	public void close() {
		// TODO Auto-generated method stub
		super.close();
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#flush()
	 */
	@Override
	public void flush() {
		// TODO Auto-generated method stub
		super.flush();
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#format(java.util.Locale, java.lang.String, java.lang.Object[])
	 */
	@Override
	public PrintWriter format(Locale l, String format, Object... args) {
		// TODO Auto-generated method stub
		return super.format(l, format, args);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#format(java.lang.String, java.lang.Object[])
	 */
	@Override
	public PrintWriter format(String format, Object... args) {
		// TODO Auto-generated method stub
		return super.format(format, args);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(boolean)
	 */
	@Override
	public void print(boolean b) {
		// TODO Auto-generated method stub
		super.print(b);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(char)
	 */
	@Override
	public void print(char c) {
		// TODO Auto-generated method stub
		super.print(c);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(char[])
	 */
	@Override
	public void print(char[] s) {
		// TODO Auto-generated method stub
		super.print(s);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(double)
	 */
	@Override
	public void print(double d) {
		// TODO Auto-generated method stub
		super.print(d);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(float)
	 */
	@Override
	public void print(float f) {
		// TODO Auto-generated method stub
		super.print(f);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(int)
	 */
	@Override
	public void print(int i) {
		// TODO Auto-generated method stub
		super.print(i);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(long)
	 */
	@Override
	public void print(long l) {
		// TODO Auto-generated method stub
		super.print(l);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(java.lang.Object)
	 */
	@Override
	public void print(Object obj) {
		// TODO Auto-generated method stub
		super.print(obj);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#print(java.lang.String)
	 */
	@Override
	public void print(String s) {
		// TODO Auto-generated method stub
		super.print(s);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#printf(java.util.Locale, java.lang.String, java.lang.Object[])
	 */
	@Override
	public PrintWriter printf(Locale l, String format, Object... args) {
		// TODO Auto-generated method stub
		return super.printf(l, format, args);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#printf(java.lang.String, java.lang.Object[])
	 */
	@Override
	public PrintWriter printf(String format, Object... args) {
		// TODO Auto-generated method stub
		return super.printf(format, args);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println()
	 */
	@Override
	public void println() {
		// TODO Auto-generated method stub
		super.println();
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(boolean)
	 */
	@Override
	public void println(boolean x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(char)
	 */
	@Override
	public void println(char x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(char[])
	 */
	@Override
	public void println(char[] x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(double)
	 */
	@Override
	public void println(double x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(float)
	 */
	@Override
	public void println(float x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(int)
	 */
	@Override
	public void println(int x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(long)
	 */
	@Override
	public void println(long x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(java.lang.Object)
	 */
	@Override
	public void println(Object x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#println(java.lang.String)
	 */
	@Override
	public void println(String x) {
		// TODO Auto-generated method stub
		super.println(x);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#setError()
	 */
	@Override
	protected void setError() {
		// TODO Auto-generated method stub
		super.setError();
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#write(char[], int, int)
	 */
	@Override
	public void write(char[] buf, int off, int len) {
		try {
			this.responseWriter.write(buf, off, len);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#write(char[])
	 */
	@Override
	public void write(char[] buf) {
		try {
			this.responseWriter.write(buf);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#write(int)
	 */
	@Override
	public void write(int c) {
		// TODO Auto-generated method stub
		super.write(c);
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#write(java.lang.String, int, int)
	 */
	@Override
	public void write(String s, int off, int len) {
		try {
			this.responseWriter.write(s, off, len);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.PrintWriter#write(java.lang.String)
	 */
	@Override
	public void write(String s) {
		try {
			this.responseWriter.write(s);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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
