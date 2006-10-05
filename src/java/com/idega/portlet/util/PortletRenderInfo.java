/*
 * $Id: PortletRenderInfo.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet.util;

import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;


/**
 * <p>
 * TODO tryggvil Describe Type PortletRenderInfo
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class PortletRenderInfo {

	
	private RenderRequest request;
	private RenderResponse response;
	
	/**
	 * 
	 */
	public PortletRenderInfo(RenderRequest request,RenderResponse response) {
		this.request=request;
		this.response=response;
	}

	
	/**
	 * @return Returns the request.
	 */
	public RenderRequest getRequest() {
		return request;
	}

	
	/**
	 * @param request The request to set.
	 */
	void setRequest(RenderRequest request) {
		this.request = request;
	}

	
	/**
	 * @return Returns the response.
	 */
	public RenderResponse getResponse() {
		return response;
	}

	
	/**
	 * @param response The response to set.
	 */
	void setResponse(RenderResponse response) {
		this.response = response;
	}
}
