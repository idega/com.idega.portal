/*
 * $Id: IWPortalContext.java,v 1.2 2007/04/20 23:31:09 eiki Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.util;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.faces.context.FacesContext;
import javax.portlet.PortalContext;
import javax.portlet.PortletMode;
import javax.portlet.WindowState;
import javax.servlet.ServletContext;

import com.idega.idegaweb.IWMainApplication;


/**
 * <p>
 * TODO tryggvil Describe Type IWPortalContext
 * </p>
 *  Last modified: $Date: 2007/04/20 23:31:09 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class IWPortalContext implements PortalContext {

	private String portalInfo = "idegaWeb Portal";
	private IWMainApplication iwma;

	private static Vector portalModes;
	private static Vector windowStates;
	
	static{
		portalModes.add(PortletMode.VIEW);
		
		windowStates.add(WindowState.NORMAL);
	}
	
	/**
	 * 
	 */
	public IWPortalContext(ServletContext context) {
		iwma = IWMainApplication.getIWMainApplication(context);
	}
	
	/**
	 * 
	 */
	public IWPortalContext(FacesContext context) {
		iwma = IWMainApplication.getIWMainApplication(context);
	}
	
	private Hashtable getPortalProperties(){
		Hashtable portalproperties = (Hashtable) iwma.getAttribute("portalpoperties");
		if(portalproperties==null){
			portalproperties = new Hashtable();
			iwma.setAttribute("portalproperties",portalproperties);
		}
		return portalproperties;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortalContext#getProperty(java.lang.String)
	 */
	public String getProperty(String arg0) {
		// TODO Auto-generated method stub
		return (String) getPortalProperties().get(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortalContext#getPropertyNames()
	 */
	public Enumeration getPropertyNames() {
		// TODO Auto-generated method stub
		return getPortalProperties().keys();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortalContext#getSupportedPortletModes()
	 */
	public Enumeration getSupportedPortletModes() {
		// TODO Auto-generated method stub
		return portalModes.elements();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortalContext#getSupportedWindowStates()
	 */
	public Enumeration getSupportedWindowStates() {
		// TODO Auto-generated method stub
		return windowStates.elements();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortalContext#getPortalInfo()
	 */
	public String getPortalInfo() {
		// TODO Auto-generated method stub
		return portalInfo;
	}
}
