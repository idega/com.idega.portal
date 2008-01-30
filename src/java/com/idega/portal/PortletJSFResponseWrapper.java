/*
 * Created on 30.3.2004
 */
package com.idega.portal;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.faces.context.ResponseWriter;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

/**
 * @author tryggvil
 */
public class PortletJSFResponseWrapper extends HttpServletResponseWrapper {

	protected HttpServletResponse origResponse = null;
	protected ServletOutputStream stream = null;
	protected PrintWriter writer = null;
	private ResponseWriter responseWriter;

	public PortletJSFResponseWrapper(HttpServletResponse response,ResponseWriter responseWriter) {
		super(response);
		this.origResponse = response;
		this.responseWriter=responseWriter;
	}

	public ServletOutputStream createOutputStream() throws IOException {
		return (new PortletJSFResponseStream(this.responseWriter));
	}

	public void finishResponse() {
		try {
			if (this.writer != null) {
				this.writer.close();
			}
			else {
				if (this.stream != null) {
					this.stream.close();
				}
			}
		}
		catch (IOException e) {
		}
	}

	public void flushBuffer() throws IOException {
		this.stream.flush();
	}

	public ServletOutputStream getOutputStream() throws IOException {
		if (this.writer != null) {
			throw new IllegalStateException("getWriter() has already been called!");
		}

		if (this.stream == null) {
			this.stream = createOutputStream();
		}
		return (this.stream);
	}

	public PrintWriter getWriter() throws IOException {
		if (this.writer != null) {
			return (this.writer);
		}

		if (this.stream != null) {
			throw new IllegalStateException("getOutputStream() has already been called!");
		}

		this.stream = createOutputStream();
		this.writer = new PortletJSFPrintWriter(this.stream,this.responseWriter);
		return (this.writer);
	}

	public void setContentLength(int length) {
	}
}