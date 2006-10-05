/*
 * $Id: PortletContainerService.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
 * Created on 13.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet.pluto;

import java.util.Properties;
import javax.servlet.ServletConfig;
import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.portalImpl.core.PortletContainerFactory;
import org.apache.pluto.services.ContainerService;
import org.apache.pluto.services.PortletContainerEnvironment;


/**
 * <p>
 * TODO tryggvil Describe Type PortletContainerService
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class PortletContainerService implements PortletContainerEnvironment{

	private static PortletContainer portletContainer;
	
	/**
	 * 
	 */
	public PortletContainerService() {
		super();
		// TODO Auto-generated constructor stub
	}
	
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
	
	/* (non-Javadoc)
	 * @see org.apache.pluto.services.PortletContainerEnvironment#getContainerService(java.lang.Class)
	 */
	public ContainerService getContainerService(Class serviceClass) {
		// TODO Auto-generated method stub
		return null;
	}
}
