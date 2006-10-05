/*
 * $Id: PortletUtils.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
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
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.om.window.PortletWindow;
import com.idega.portlet.pluto.PortletContainerService;


/**
 * <p>
 * TODO tryggvil Describe Type PortletUtils
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class PortletUtils {

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
	 * @return
	 * @throws PortletContainerException 
	 * @throws IOException 
	 * @throws PortletException 
	 */
	public static void renderPortlet(FacesContext context) throws PortletException, IOException, PortletContainerException {
		context.getExternalContext().getContext();
		
		ServletConfig config = null;
		PortletContainer container = PortletContainerService.getPortletContainer(config);
		
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();
		
		PortletWindow window = null;
		
		container.renderPortlet(window,request,response);
		
	}
	
	
}
