/*
 * $Id: PortletUtils.java,v 1.2 2006/10/05 22:23:24 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet.util;

import java.io.IOException;
import javax.faces.context.FacesContext;
import javax.portlet.PortletException;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.pluto.OptionalContainerServices;
import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.PortletContainerFactory;
import org.apache.pluto.PortletWindow;
import org.apache.pluto.RequiredContainerServices;
import org.apache.pluto.driver.config.DriverConfiguration;
import org.apache.pluto.driver.config.impl.DriverConfigurationImpl;
import org.apache.pluto.driver.services.container.ContainerServicesImpl;
import org.apache.pluto.driver.services.container.PortalCallbackServiceImpl;
import org.apache.pluto.driver.services.container.PortalContextImpl;
import org.apache.pluto.driver.services.impl.resource.PortletRegistryServiceImpl;
import org.apache.pluto.driver.services.impl.resource.PropertyConfigServiceImpl;
import org.apache.pluto.driver.services.impl.resource.RenderConfigServiceImpl;
import org.apache.pluto.driver.services.impl.resource.SupportedModesServiceImpl;
import org.apache.pluto.driver.services.portal.PortletRegistryService;
import org.apache.pluto.driver.services.portal.PropertyConfigService;
import org.apache.pluto.driver.services.portal.RenderConfigService;
import org.apache.pluto.driver.services.portal.SupportedModesService;
import org.apache.pluto.driver.services.portal.admin.PortletRegistryAdminService;
import org.apache.pluto.driver.url.PortalURLParser;
import org.apache.pluto.driver.url.impl.PortalURLParserImpl;
import org.apache.pluto.spi.PortalCallbackService;
import com.idega.portlet.pluto.PortletContainerService;
import com.idega.portlet.pluto.util.IWDriverConfiguration;


/**
 * <p>
 * TODO tryggvil Describe Type PortletUtils
 * </p>
 *  Last modified: $Date: 2006/10/05 22:23:24 $ by $Author: tryggvil $
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
		
		//
//		 Step 1) Create an instance of the PortletContainerService
		//
		RequiredContainerServices impl = getPortletContainerServies(servletContext);
		//
//		 Step 2) Request a new container from the container factory
		//
		PortletContainerFactory factory = PortletContainerFactory.getInstance();
		PortletContainer container = factory.createContainer("My Container Name", impl);
		//
//		 Step 3) Initialize the Container with the embedding
//		         application's ServletContext
		//
		container.init(servletContext );
		
		return container;
	}

	private static RequiredContainerServices getPortletContainerServies(ServletContext servletContext) {
		if(containerServices==null){
			PortletRegistryService portletRegistry = new PortletRegistryServiceImpl();
			PortalURLParser portalUrlParser = PortalURLParserImpl.getParser();
			PropertyConfigService propertyConfigService = new PropertyConfigServiceImpl();
			RenderConfigService renderConfigService = new RenderConfigServiceImpl();
			SupportedModesService supportedModesService = new SupportedModesServiceImpl(portletRegistry,propertyConfigService);
			PortalCallbackService portalCallbackService = new PortalCallbackServiceImpl();
			//DriverConfiguration driverConfiguration = new IWDriverConfiguration(servletContext);
			DriverConfiguration driverConfiguration = new DriverConfigurationImpl(portalUrlParser,propertyConfigService,portletRegistry,renderConfigService,portalCallbackService ,supportedModesService);
			//containerServices = new PortletContainerService(servletContext);
			containerServices = new ContainerServicesImpl(new PortalContextImpl(driverConfiguration),driverConfiguration);
		}
		return containerServices;
	}
	
	
}
