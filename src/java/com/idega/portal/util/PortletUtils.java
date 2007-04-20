/*
 * $Id: PortletUtils.java,v 1.2 2007/04/20 23:31:08 eiki Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.util;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.portlet.PortletException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.PortletContainerFactory;
import org.apache.pluto.PortletWindow;
import org.apache.pluto.RequiredContainerServices;
import org.apache.pluto.driver.config.DriverConfiguration;
import org.apache.pluto.driver.config.impl.DriverConfigurationImpl;
import org.apache.pluto.driver.core.PortalRequestContext;
import org.apache.pluto.driver.core.PortletWindowImpl;
import org.apache.pluto.driver.services.container.ContainerServicesImpl;
import org.apache.pluto.driver.services.container.PortalCallbackServiceImpl;
import org.apache.pluto.driver.services.container.PortalContextImpl;
import org.apache.pluto.driver.services.impl.resource.PropertyConfigServiceImpl;
import org.apache.pluto.driver.services.impl.resource.RenderConfigServiceImpl;
import org.apache.pluto.driver.services.impl.resource.SupportedModesServiceImpl;
import org.apache.pluto.driver.services.impl.resource.SupportedWindowStateServiceImpl;
import org.apache.pluto.driver.services.portal.PortletWindowConfig;
import org.apache.pluto.driver.services.portal.PropertyConfigService;
import org.apache.pluto.driver.services.portal.RenderConfigService;
import org.apache.pluto.driver.services.portal.SupportedModesService;
import org.apache.pluto.driver.services.portal.SupportedWindowStateService;
import org.apache.pluto.driver.url.PortalURL;
import org.apache.pluto.driver.url.PortalURLParser;
import org.apache.pluto.driver.url.impl.PortalURLParserImpl;
import org.apache.pluto.spi.PortalCallbackService;

import com.idega.portal.pluto.PortletContainerService;


/**
 * <p>
 * TODO tryggvil Describe Type PortletUtils
 * </p>
 *  Last modified: $Date: 2007/04/20 23:31:08 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class PortletUtils {

	private static RequiredContainerServices containerServices;

	/**
	 * 
	 */
	public PortletUtils() {
		super();
		// TODO Auto-generated constructor stub
	}

	
	public static void renderPortlet(FacesContext context, PortletWindowConfig windowConfig) throws PortletException, IOException, PortletContainerException {

        // Retrieve the current portal URL.
		
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();
		
        // Retrieve the current portal URL.
		
        PortalRequestContext portalEnv = PortalRequestContext.getContext(request);
        PortalURL portalURL = portalEnv.getRequestedPortalURL();
		
        renderPortlet(context,windowConfig,portalURL);
        
	}
	
	/**
	 * <p>
	 * TODO tryggvil describe method createRenderInfo
	 * </p>
	 * @param context
	 * @param window 
	 * @return
	 * @throws PortletContainerException 
	 * @throws IOException 
	 * @throws PortletException 
	 */
	public static void renderPortlet(FacesContext context, PortletWindowConfig windowConfig,PortalURL portalURL) throws PortletException, IOException, PortletContainerException {

        // Create the portlet window to render.
        PortletWindow window = new PortletWindowImpl(windowConfig, portalURL);
        
        renderPortlet(context, window);
        
	}	
	
	
	
	/**
	 * <p>
	 * TODO tryggvil describe method createRenderInfo
	 * </p>
	 * @param context
	 * @param window 
	 * @return
	 * @throws PortletContainerException 
	 * @throws IOException 
	 * @throws PortletException 
	 */
	public static void renderPortlet(FacesContext context, PortletWindow window) throws PortletException, IOException, PortletContainerException {

		ServletContext servletContext = (ServletContext) context.getExternalContext().getContext();
		PortletContainer container = getPortletContainer(servletContext);
		
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();
		
		container.doRender(window,request,response);
		
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getPortletContainer
	 * </p>
	 * @param servletContext
	 * @return
	 * @throws PortletContainerException
	 */
	public static PortletContainer getPortletContainer(ServletContext servletContext) throws PortletContainerException {
		//PortletContainer container = PortletContainerService.getPortletContainer(config);
		
		//TODO STORE the container it's not supposed to be created everytime
		//
//		 Step 1) Create an instance of the PortletContainerService
		//
		PortletContainerService impl = new PortletContainerService();
		//RequiredContainerServices impl = getPortletContainerServices(servletContext);
		//
//		 Step 2) Request a new container from the container factory
		//
		PortletContainerFactory factory = PortletContainerFactory.getInstance();
		PortletContainer container = factory.createContainer("IdegaWeb Portlets", impl);
		//
//		 Step 3) Initialize the Container with the embedding
//		         application's ServletContext
		//
		container.init(servletContext);
		
		return container;
	}

	
	private static RequiredContainerServices getPortletContainerServices(ServletContext servletContext) {
		if(containerServices==null){
		//	PortletRegistryService portletRegistry = new IWPortletRegistryService();
			PortalURLParser portalUrlParser = PortalURLParserImpl.getParser();
			PropertyConfigService propertyConfigService = new PropertyConfigServiceImpl();
			RenderConfigService renderConfigService = new RenderConfigServiceImpl();
			SupportedModesService supportedModesService = new SupportedModesServiceImpl(propertyConfigService);
			PortalCallbackService portalCallbackService = new PortalCallbackServiceImpl();
			SupportedWindowStateService supportedWindowStateService = new SupportedWindowStateServiceImpl(propertyConfigService);
			//DriverConfiguration driverConfiguration = new IWDriverConfiguration(servletContext);
			DriverConfiguration driverConfiguration = new DriverConfigurationImpl(portalUrlParser,propertyConfigService,renderConfigService,supportedModesService,supportedWindowStateService,portalCallbackService);
			//containerServices = new PortletContainerService(servletContext);
			containerServices = new ContainerServicesImpl(new PortalContextImpl(driverConfiguration),driverConfiguration);
		}
		return containerServices;
	}
	
	
}
