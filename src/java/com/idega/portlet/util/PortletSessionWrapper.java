/*
 * $Id: PortletSessionWrapper.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet.util;

import java.util.Enumeration;
import javax.faces.context.FacesContext;
import javax.portlet.PortletContext;
import javax.portlet.PortletSession;


/**
 * <p>
 * TODO tryggvil Describe Type PortletSessionWrapper
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class PortletSessionWrapper implements PortletSession {

	private FacesContext facesContext;
	private PortletContext portletContext;

	/**
	 * @param facesContext 
	 * 
	 */
	public PortletSessionWrapper(FacesContext facesContext,PortletContext portletContext) {
		this.facesContext=facesContext;
		this.portletContext=portletContext;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getAttribute(java.lang.String)
	 */
	public Object getAttribute(String key) {
		return getAttribute(key,PORTLET_SCOPE);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getAttribute(java.lang.String, int)
	 */
	public Object getAttribute(String key, int scope) {
		
		if(scope==PORTLET_SCOPE){
			Object theReturn = facesContext.getExternalContext().getSessionMap().get(key);
			if(theReturn!=null){
				return theReturn;
			}
		}
		else if(scope==APPLICATION_SCOPE){
			Object theReturn = facesContext.getExternalContext().getApplicationMap().get(key);
			if(theReturn!=null){
				return theReturn;
			}
		}
		
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getAttributeNames()
	 */
	public Enumeration getAttributeNames() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getAttributeNames(int)
	 */
	public Enumeration getAttributeNames(int arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getCreationTime()
	 */
	public long getCreationTime() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getId()
	 */
	public String getId() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getLastAccessedTime()
	 */
	public long getLastAccessedTime() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getMaxInactiveInterval()
	 */
	public int getMaxInactiveInterval() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#invalidate()
	 */
	public void invalidate() {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#isNew()
	 */
	public boolean isNew() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#removeAttribute(java.lang.String)
	 */
	public void removeAttribute(String arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#removeAttribute(java.lang.String, int)
	 */
	public void removeAttribute(String arg0, int arg1) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#setAttribute(java.lang.String, java.lang.Object)
	 */
	public void setAttribute(String arg0, Object arg1) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#setAttribute(java.lang.String, java.lang.Object, int)
	 */
	public void setAttribute(String arg0, Object arg1, int arg2) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#setMaxInactiveInterval(int)
	 */
	public void setMaxInactiveInterval(int arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletSession#getPortletContext()
	 */
	public PortletContext getPortletContext() {
		return portletContext;
	}
}
