/*
 * $Id: IWPortletContainerService.java,v 1.1 2007/04/22 14:59:25 eiki Exp $
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
 *  Last modified: $Date: 2007/04/22 14:59:25 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class IWPortletContainerService implements RequiredContainerServices{//PortletContainerEnvironment{

	private PortletContainer container;
	PortalContext context;
	private ServletContext servletContext;
	
	/**
	 * 
	 */
	public IWPortletContainerService(ServletContext servletContext) {
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

	public IWPortletContainerService() {
		super();
	}
	
	public void setPortletContainer(PortletContainer container){
		this.container = container;
	}
	
	public PortletContainer getPortletContainer(){
		return this.container;
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
