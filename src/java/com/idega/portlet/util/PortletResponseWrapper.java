/*
 * $Id: PortletResponseWrapper.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Locale;
import javax.faces.context.FacesContext;
import javax.portlet.PortletURL;
import javax.portlet.RenderResponse;


/**
 * <p>
 * TODO tryggvil Describe Type PortletResponseWrapper
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class PortletResponseWrapper implements RenderResponse {

	/**
	 * @param context 
	 * 
	 */
	public PortletResponseWrapper(FacesContext context) {
		super();
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getContentType()
	 */
	public String getContentType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#createRenderURL()
	 */
	public PortletURL createRenderURL() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#createActionURL()
	 */
	public PortletURL createActionURL() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getNamespace()
	 */
	public String getNamespace() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#setTitle(java.lang.String)
	 */
	public void setTitle(String arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#setContentType(java.lang.String)
	 */
	public void setContentType(String arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getCharacterEncoding()
	 */
	public String getCharacterEncoding() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getWriter()
	 */
	public PrintWriter getWriter() throws IOException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getLocale()
	 */
	public Locale getLocale() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#setBufferSize(int)
	 */
	public void setBufferSize(int arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getBufferSize()
	 */
	public int getBufferSize() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#flushBuffer()
	 */
	public void flushBuffer() throws IOException {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#resetBuffer()
	 */
	public void resetBuffer() {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#isCommitted()
	 */
	public boolean isCommitted() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#reset()
	 */
	public void reset() {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.RenderResponse#getPortletOutputStream()
	 */
	public OutputStream getPortletOutputStream() throws IOException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletResponse#addProperty(java.lang.String, java.lang.String)
	 */
	public void addProperty(String arg0, String arg1) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletResponse#setProperty(java.lang.String, java.lang.String)
	 */
	public void setProperty(String arg0, String arg1) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletResponse#encodeURL(java.lang.String)
	 */
	public String encodeURL(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}
}
