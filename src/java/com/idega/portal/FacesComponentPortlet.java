/*
 * $Id: FacesComponentPortlet.java,v 1.1 2007/04/20 22:25:39 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Locale;
import java.util.ResourceBundle;
import javax.faces.component.UIComponent;
import javax.portlet.ActionRequest;
import javax.portlet.ActionResponse;
import javax.portlet.GenericPortlet;
import javax.portlet.Portlet;
import javax.portlet.PortletConfig;
import javax.portlet.PortletContext;
import javax.portlet.PortletException;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;


/**
 * <p>
 * A Portlet that wraps a simple instance of a JSF UIComponent
 * </p>
 *  Last modified: $Date: 2007/04/20 22:25:39 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
public class FacesComponentPortlet extends GenericPortlet implements Portlet {

	private UIComponent wrappedComponent;
	
	/**
	 * 
	 */
	public FacesComponentPortlet() {
		super();
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param text
	 */
	public FacesComponentPortlet(UIComponent wrappedcomponent) {
		// TODO Auto-generated constructor stub
		this.wrappedComponent=wrappedcomponent;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#destroy()
	 */
	public void destroy() {
		// TODO Auto-generated method stub
		super.destroy();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#doDispatch(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
	 */
	protected void doDispatch(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.doDispatch(arg0, arg1);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#doEdit(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
	 */
	protected void doEdit(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.doEdit(arg0, arg1);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#doHelp(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
	 */
	protected void doHelp(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.doHelp(arg0, arg1);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#doView(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
	 */
	protected void doView(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.doView(arg0, arg1);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getInitParameter(java.lang.String)
	 */
	public String getInitParameter(String arg0) {
		// TODO Auto-generated method stub
		return super.getInitParameter(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getInitParameterNames()
	 */
	public Enumeration getInitParameterNames() {
		// TODO Auto-generated method stub
		return super.getInitParameterNames();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getPortletConfig()
	 */
	public PortletConfig getPortletConfig() {
		// TODO Auto-generated method stub
		return super.getPortletConfig();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getPortletContext()
	 */
	public PortletContext getPortletContext() {
		// TODO Auto-generated method stub
		return super.getPortletContext();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getPortletName()
	 */
	public String getPortletName() {
		// TODO Auto-generated method stub
		return super.getPortletName();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getResourceBundle(java.util.Locale)
	 */
	public ResourceBundle getResourceBundle(Locale arg0) {
		// TODO Auto-generated method stub
		return super.getResourceBundle(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#getTitle(javax.portlet.RenderRequest)
	 */
	protected String getTitle(RenderRequest arg0) {
		// TODO Auto-generated method stub
		return super.getTitle(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#init()
	 */
	public void init() throws PortletException {
		// TODO Auto-generated method stub
		super.init();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#init(javax.portlet.PortletConfig)
	 */
	public void init(PortletConfig arg0) throws PortletException {
		// TODO Auto-generated method stub
		super.init(arg0);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#processAction(javax.portlet.ActionRequest, javax.portlet.ActionResponse)
	 */
	public void processAction(ActionRequest arg0, ActionResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.processAction(arg0, arg1);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.GenericPortlet#render(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
	 */
	public void render(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException {
		// TODO Auto-generated method stub
		super.render(arg0, arg1);
	}
}
