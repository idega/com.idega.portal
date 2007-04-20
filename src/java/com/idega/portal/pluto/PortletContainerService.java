/*
 * $Id: PortletContainerService.java,v 1.2 2007/04/20 23:31:09 eiki Exp $
 * Created on 13.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.pluto;

import javax.portlet.PortalContext;
import javax.servlet.ServletContext;

import org.apache.pluto.PortletContainer;
import org.apache.pluto.RequiredContainerServices;
import org.apache.pluto.spi.PortalCallbackService;

import com.idega.portal.pluto.util.IWPortalCallbackService;
import com.idega.portal.util.IWPortalContext;


/**
 * <p>
 * TODO tryggvil Describe Type PortletContainerService
 * </p>
 *  Last modified: $Date: 2007/04/20 23:31:09 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class PortletContainerService implements RequiredContainerServices{//PortletContainerEnvironment{

	private static PortletContainer portletContainer;
	PortalContext context;
	private ServletContext servletContext;
	
	/**
	 * 
	 */
	public PortletContainerService(ServletContext servletContext) {
		this.servletContext = servletContext;
	}

	public PortalCallbackService getPortalCallbackService() {
		PortalCallbackService cbService = new IWPortalCallbackService();
		return cbService;
	}

	public PortalContext getPortalContext() {
		if(context==null){
			context = new IWPortalContext(this.servletContext);
		}
		return context;
	}

	public PortletContainerService() {
		super();
	}
	
	/*
	public static PortletContainer getPortletContainer(ServletConfig config){
		if(portletContainer==null){
			PortletContainerEnvironment environment = new PortletContainerService();
			Properties properties = new Properties();
			portletContainer = PortletContainerFactory.getPortletContainer();
			try {
				portletContainer.init("idegaWeb",config,environment,properties);
			}
			catch (PortletContainerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return portletContainer;
	}
	*/
	
	/*
	public ContainerService getContainerService(Class serviceClass) {
		// TODO Auto-generated method stub
		return null;
	}
	*/
}
