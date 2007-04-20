/**
 * $Id: IWPortletRegistryService.java,v 1.2 2007/04/20 23:31:10 eiki Exp $
 * Created in 2006 by tryggvil
 * 
 * Copyright (C) 2000-2006 Idega Software hf. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega hf. Use is subject to
 * license terms.
 */
package com.idega.portal.pluto;

import java.io.InputStream;
import java.util.Iterator;

import javax.portlet.PortletConfig;
import javax.portlet.PortletContext;
import javax.servlet.ServletContext;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.descriptors.portlet.PortletAppDD;
import org.apache.pluto.descriptors.portlet.PortletDD;
import org.apache.pluto.driver.config.DriverConfigurationException;
import org.apache.pluto.driver.services.impl.resource.ResourceConfig;
import org.apache.pluto.driver.services.impl.resource.ResourceConfigReader;
import org.apache.pluto.driver.services.portal.admin.DriverAdministrationException;
import org.apache.pluto.driver.services.portal.admin.PortletRegistryAdminService;
import org.apache.pluto.internal.PortletDescriptorRegistry;
import org.apache.pluto.spi.optional.PortletRegistryListener;
import org.apache.pluto.spi.optional.PortletRegistryService;

import com.idega.portal.pluto.util.ConfigUtil;

/**
 * <p>
 * TODO tryggvil Describe Type IWPortletRegistryService
 * </p>
 * Last modified: $Date: 2007/04/20 23:31:10 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class IWPortletRegistryService implements PortletRegistryService, PortletRegistryAdminService {



    private static final Log LOG = LogFactory.getLog(PortletRegistryService.class);
    private ResourceConfig config;
    private ServletContext servletContext;
    private PortletContainer container;

	// Constructor -------------------------------------------------------------
	/**
	 * 
	 * Default no-arg constructor.
	 * 
	 */
	public IWPortletRegistryService() {
		// Do nothing.
	}

	// DriverConfigurationService Impl -----------------------------------------
	/**
	 * 
	 * Initialization Lifecycle Method
	 * 
	 * @param servletContext
	 *            the servlet context.
	 * 
	 */
	public void init(ServletContext servletContext) throws DriverConfigurationException {
		try {
			this.servletContext = servletContext;
			InputStream in = ConfigUtil.getConfigFileInputStream();
			this.config = ResourceConfigReader.getFactory().parse(in);
		}
		catch (Exception ex) {
			throw new DriverConfigurationException(ex);
		}
	}

	public void destroy() throws DriverConfigurationException {
		config = null;
		servletContext = null;
	}


	// PortletRegistryAdminService Impl ----------------------------------------
//	public void addPortletApplication(String contextPath) throws DriverAdministrationException {
//		if (contextPath == null) {
//			throw new IllegalArgumentException("Portlet application context path cannot be null.");
//		}
//		try {
//			PortletApplicationConfig portletAppConfig = new PortletApplicationConfig();
//			portletAppConfig.setContextPath(contextPath);
//			ServletContext portletAppServletContext = servletContext.getContext(contextPath);
//			if (portletAppServletContext == null) {
//				throw new DriverAdministrationException("Unable to locate servlet context: " + contextPath
//						+ ": ensure that crossContext support is enabled "
//						+ "and the portlet application has been deployed.");
//			}
//			PortletAppDD portletAppDD = getPortletDescriptor(portletAppServletContext);
//			if (portletAppDD == null) {
//				throw new DriverAdministrationException("Unable to retrieve portlet application descriptor from "
//						+ contextPath + ": ensure that the portlet application " + "has been de	ployed.");
//			}
//			for (Iterator it = portletAppDD.getPortlets().iterator(); it.hasNext();) {
//				PortletDD portletDD = (PortletDD) it.next();
//				PortletWindowConfig portletWindowConfig = new PortletWindowConfig();
//				portletWindowConfig.setContextPath(contextPath);
//				portletWindowConfig.setPortletName(portletDD.getPortletName());
//				portletAppConfig.addPortlet(portletWindowConfig);
//			}
//			config.addPortletApp(portletAppConfig);
//		}
//		catch (PortletContainerException ex) {
//			throw new DriverAdministrationException("Unable to add portlet application from " + contextPath, ex);
//		}
//	}


	// Private Methods ---------------------------------------------------------
	private PortletAppDD getPortletDescriptor(ServletContext context) throws PortletContainerException {
		return PortletDescriptorRegistry.getRegistry().getPortletAppDD(context);
	}
	
	
	// IWPortletRegistryService addition
//	public void addPortletApplication(PortletApplicationConfig portletAppConfig) throws DriverAdministrationException {
//		config.addPortletApp(portletAppConfig);
//	}
//
//	// IWPortletRegistryService addition
//	public void addPortlet(PortletWindowConfig windowconfig) throws DriverAdministrationException {
//		PortletApplicationConfig portletAppConfig = new PortletApplicationConfig();
//		portletAppConfig.addPortlet(windowconfig);
//		addPortletApplication(portletAppConfig);
//	}

	public void addPortletRegistryListener(PortletRegistryListener arg0) {
		// TODO Auto-generated method stub
		
	}

	public PortletAppDD getPortletApplicationDescriptor(String arg0) throws PortletContainerException {
		// TODO Auto-generated method stub
		return null;
	}

	public PortletConfig getPortletConfig(String arg0, String arg1) throws PortletContainerException {
		// TODO Auto-generated method stub
		return null;
	}

	public PortletContext getPortletContext(String arg0) throws PortletContainerException {
		// TODO Auto-generated method stub
		return null;
	}

	public PortletDD getPortletDescriptor(String arg0, String arg1) throws PortletContainerException {
		// TODO Auto-generated method stub
		return null;
	}

	public Iterator getRegisteredPortletApplicationIds() {
		// TODO Auto-generated method stub
		return null;
	}

	public Iterator getRegisteredPortletApplications() {
		// TODO Auto-generated method stub
		return null;
	}

	public void removePortletRegistryListener(PortletRegistryListener arg0) {
		// TODO Auto-generated method stub
		
	}

	public void addPortletApplication(String arg0) throws DriverAdministrationException {
		// TODO Auto-generated method stub
	}
	
}
