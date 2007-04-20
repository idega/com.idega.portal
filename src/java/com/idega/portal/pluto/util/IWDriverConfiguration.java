/**
 * $Id: IWDriverConfiguration.java,v 1.2 2007/04/20 23:31:10 eiki Exp $
 * Created in 2006 by tryggvil
 *
 * Copyright (C) 2000-2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.pluto.util;

import java.util.Collection;

import javax.servlet.ServletContext;

import org.apache.pluto.driver.config.DriverConfiguration;
import org.apache.pluto.driver.config.DriverConfigurationException;
import org.apache.pluto.driver.services.portal.PageConfig;
import org.apache.pluto.driver.services.portal.PortletWindowConfig;
import org.apache.pluto.driver.url.PortalURLParser;
import org.apache.pluto.driver.url.impl.PortalURLParserImpl;
import org.apache.pluto.spi.PortalCallbackService;
import org.apache.pluto.spi.optional.PortletPreferencesService;


/**
 * <p>
 * TODO tryggvil Describe Type IWDriverConfiguration
 * </p>
 *  Last modified: $Date: 2007/04/20 23:31:10 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class IWDriverConfiguration implements DriverConfiguration {

	private String containerName = "idegaWeb Portal";
	private PortalCallbackService callbackService;
	private PortalURLParser portalUrlParser;
	private String portalVersion="3.5.0";

	public IWDriverConfiguration(ServletContext servletContext) {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#destroy()
	 */
	public void destroy() throws DriverConfigurationException {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getContainerName()
	 */
	public String getContainerName() {
		return containerName ;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPageConfig(java.lang.String)
	 */
	public PageConfig getPageConfig(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPages()
	 */
	public Collection getPages() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortalCallbackService()
	 */
	public PortalCallbackService getPortalCallbackService() {
		if(callbackService==null){
			callbackService = new IWPortalCallbackService();
		}
		return callbackService;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortalName()
	 */
	public String getPortalName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortalUrlParser()
	 */
	public PortalURLParser getPortalUrlParser() {
		if(portalUrlParser==null){
			portalUrlParser = PortalURLParserImpl.getParser();
		}
		return portalUrlParser;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortalVersion()
	 */
	public String getPortalVersion() {
		// TODO Auto-generated method stub
		return portalVersion;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortletApplications()
	 */
	public Collection getPortletApplications() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortletPreferencesService()
	 */
	public PortletPreferencesService getPortletPreferencesService() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getPortletWindowConfig(java.lang.String)
	 */
	public PortletWindowConfig getPortletWindowConfig(String arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getSupportedPortletModes()
	 */
	public Collection getSupportedPortletModes() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#getSupportedWindowStates()
	 */
	public Collection getSupportedWindowStates() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#init(javax.servlet.ServletContext)
	 */
	public void init(ServletContext arg0) throws DriverConfigurationException {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#isPortletModeSupported(java.lang.String, java.lang.String)
	 */
	public boolean isPortletModeSupported(String arg0, String arg1) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#isPortletModeSupportedByPortal(java.lang.String)
	 */
	public boolean isPortletModeSupportedByPortal(String arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.driver.config.DriverConfiguration#isPortletModeSupportedByPortlet(java.lang.String, java.lang.String)
	 */
	public boolean isPortletModeSupportedByPortlet(String arg0, String arg1) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isWindowStateSupported(String arg0, String arg1) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isWindowStateSupportedByPortal(String arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isWindowStateSupportedByPortlet(String arg0, String arg1) {
		// TODO Auto-generated method stub
		return false;
	}
}
