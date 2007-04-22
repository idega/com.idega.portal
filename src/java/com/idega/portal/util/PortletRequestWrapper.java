/*
 * $Id: PortletRequestWrapper.java,v 1.2 2007/04/22 14:59:25 eiki Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.util;

import java.security.Principal;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;
import javax.faces.context.FacesContext;
import javax.portlet.PortalContext;
import javax.portlet.Portlet;
import javax.portlet.PortletContext;
import javax.portlet.PortletMode;
import javax.portlet.PortletPreferences;
import javax.portlet.PortletSession;
import javax.portlet.RenderRequest;
import javax.portlet.WindowState;
import javax.servlet.http.HttpServletRequest;

import org.apache.pluto.PortletContainerException;
import org.apache.pluto.internal.impl.RenderRequestImpl;

import com.idega.idegaweb.IWMainApplication;


/**
 * <p>
 * TODO tryggvil Describe Type PortletRequestWrapper
 * </p>
 *  Last modified: $Date: 2007/04/22 14:59:25 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class PortletRequestWrapper implements RenderRequest {

	HttpServletRequest servletRequest;
	PortalContext portalContext;
	PortletSession portletSession;
	private FacesContext facesContext;
	private PortletPreferences portletPreferences;
	private PortletContext portletContext;

	/**
	 * @param context
	 */
	public PortletRequestWrapper(FacesContext context,Portlet portlet) {
		// TODO Auto-generated constructor stub
		facesContext = context;
		servletRequest = (HttpServletRequest) context.getExternalContext().getRequest();
		try {
			portalContext = PortletUtils.getPortletContainer(IWMainApplication.getIWMainApplication(context).getServletContext()).getRequiredContainerServices().getPortalContext();
		} catch (PortletContainerException e) {
			e.printStackTrace();
		}
		portletPreferences = new IWPortletPreferenes(context,portlet);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#isWindowStateAllowed(javax.portlet.WindowState)
	 */
	public boolean isWindowStateAllowed(WindowState windowState) {
		// TODO Auto-generated method stub
		Enumeration states = getPortalContext().getSupportedWindowStates();
		while(states.hasMoreElements()){
			WindowState state = (WindowState) states.nextElement();
			if(state.equals(windowState)){
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#isPortletModeAllowed(javax.portlet.PortletMode)
	 */
	public boolean isPortletModeAllowed(PortletMode portletMode) {
		Enumeration modes = getPortalContext().getSupportedPortletModes();
		while(modes.hasMoreElements()){
			PortletMode mode = (PortletMode) modes.nextElement();
			if(mode.equals(portletMode)){
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPortletMode()
	 */
	public PortletMode getPortletMode() {
		//todo  always set to view for now
		return PortletMode.VIEW;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getWindowState()
	 */
	public WindowState getWindowState() {
		//todo just set to normal now so we won't get a nullpointer
		return WindowState.NORMAL;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPreferences()
	 */
	public PortletPreferences getPreferences() {
		// TODO Auto-generated method stub
		return portletPreferences;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPortletSession()
	 */
	public PortletSession getPortletSession() {
		return getPortletSession(true);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPortletSession(boolean)
	 */
	public PortletSession getPortletSession(boolean create) {
		if(create){
			PortletContext portletContext=null;
			portletSession=new PortletSessionWrapper(facesContext,portletContext);
		}
		return portletSession;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getProperty(java.lang.String)
	 */
	public String getProperty(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getProperties(java.lang.String)
	 */
	public Enumeration getProperties(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPropertyNames()
	 */
	public Enumeration getPropertyNames() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getPortalContext()
	 */
	public PortalContext getPortalContext() {
		return portalContext;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getAuthType()
	 */
	public String getAuthType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getContextPath()
	 */
	public String getContextPath() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getRemoteUser()
	 */
	public String getRemoteUser() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getUserPrincipal()
	 */
	public Principal getUserPrincipal() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#isUserInRole(java.lang.String)
	 */
	public boolean isUserInRole(String arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getAttribute(java.lang.String)
	 */
	public Object getAttribute(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getAttributeNames()
	 */
	public Enumeration getAttributeNames() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getParameter(java.lang.String)
	 */
	public String getParameter(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getParameterNames()
	 */
	public Enumeration getParameterNames() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getParameterValues(java.lang.String)
	 */
	public String[] getParameterValues(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getParameterMap()
	 */
	public Map getParameterMap() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#isSecure()
	 */
	public boolean isSecure() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#setAttribute(java.lang.String, java.lang.Object)
	 */
	public void setAttribute(String arg0, Object arg1) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#removeAttribute(java.lang.String)
	 */
	public void removeAttribute(String arg0) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getRequestedSessionId()
	 */
	public String getRequestedSessionId() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#isRequestedSessionIdValid()
	 */
	public boolean isRequestedSessionIdValid() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getResponseContentType()
	 */
	public String getResponseContentType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getResponseContentTypes()
	 */
	public Enumeration getResponseContentTypes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getLocale()
	 */
	public Locale getLocale() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getLocales()
	 */
	public Enumeration getLocales() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getScheme()
	 */
	public String getScheme() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getServerName()
	 */
	public String getServerName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletRequest#getServerPort()
	 */
	public int getServerPort() {
		// TODO Auto-generated method stub
		return 0;
	}
}
