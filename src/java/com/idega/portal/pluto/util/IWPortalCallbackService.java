/**
 * $Id: IWPortalCallbackService.java,v 1.3 2007/04/22 14:59:25 eiki Exp $
 * Created in 2006 by tryggvil
 *
 * Copyright (C) 2000-2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.pluto.util;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.pluto.PortletWindow;
import org.apache.pluto.driver.services.container.ResourceURLProviderImpl;
import org.apache.pluto.spi.PortalCallbackService;
import org.apache.pluto.spi.PortletURLProvider;
import org.apache.pluto.spi.ResourceURLProvider;


/**
 * <p>
 * TODO tryggvil Describe Type IWPortalCallbackService
 * </p>
 *  Last modified: $Date: 2007/04/22 14:59:25 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.3 $
 */
public class IWPortalCallbackService implements PortalCallbackService {

	ResourceURLProvider resourceUrlProvider;
	PortletURLProvider portletUrlProvider;
	
	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#addResponseProperty(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow, java.lang.String, java.lang.String)
	 */
	public void addResponseProperty(HttpServletRequest request, PortletWindow window, String arg2, String arg3) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#getPortletURLProvider(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow)
	 */
	public PortletURLProvider getPortletURLProvider(HttpServletRequest arg0, PortletWindow arg1) {
		if(portletUrlProvider==null){
			portletUrlProvider = new IWPortletURLProvider();
		}
		return portletUrlProvider;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#getRequestProperties(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow)
	 */
	public Map getRequestProperties(HttpServletRequest request, PortletWindow arg1) {
		return request.getParameterMap();
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#getResourceURLProvider(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow)
	 */
	public ResourceURLProvider getResourceURLProvider(HttpServletRequest request, PortletWindow window) {
		if(resourceUrlProvider==null){
			resourceUrlProvider = new ResourceURLProviderImpl(request,window);
		}
		return resourceUrlProvider;
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#setResponseProperty(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow, java.lang.String, java.lang.String)
	 */
	public void setResponseProperty(HttpServletRequest request, PortletWindow arg1, String arg2, String arg3) {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see org.apache.pluto.spi.PortalCallbackService#setTitle(javax.servlet.http.HttpServletRequest, org.apache.pluto.PortletWindow, java.lang.String)
	 */
	public void setTitle(HttpServletRequest request, PortletWindow window, String arg2) {
		// TODO Auto-generated method stub
	}
}
