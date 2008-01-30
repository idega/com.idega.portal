/*
 * $Id: PortletControl.java,v 1.4 2008/01/30 14:32:04 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.faces.context.FacesContext;
import javax.portlet.PortletException;
import javax.portlet.PortletMode;
import javax.portlet.WindowState;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;
import org.apache.pluto.PortletWindow;
import org.apache.pluto.descriptors.portlet.PortletAppDD;
import org.apache.pluto.descriptors.portlet.PortletDD;
import org.apache.pluto.driver.core.PortalRequestContext;
import org.apache.pluto.driver.core.PortletWindowImpl;
import org.apache.pluto.driver.services.portal.PortletWindowConfig;
import org.apache.pluto.driver.url.PortalURL;
import org.apache.pluto.driver.url.PortalURLParser;
import org.apache.pluto.driver.url.impl.PortalURLImpl;
import org.apache.pluto.driver.url.impl.PortalURLParserImpl;
import org.apache.pluto.internal.InternalPortletWindow;
import org.apache.pluto.internal.impl.InternalPortletWindowImpl;

import com.idega.portal.util.PortletUtils;
import com.idega.presentation.IWContext;
import com.idega.webface.WFBlock;


/**
 * <p>
 * JSF Component to control a wrapped Portlet instance.
 * </p>
 *  Last modified: $Date: 2008/01/30 14:32:04 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.4 $
 */
public class PortletControl extends WFBlock {

	//private transient Portlet portlet;

	String portletAppId;
	String portletName;
	
	WindowState state=WindowState.NORMAL;
	PortletMode mode=PortletMode.VIEW;
	
	private transient PortletDD portlet;
	
	/**
	 * 
	 */
	public PortletControl() {
		this("Portlet");
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param titleBarText
	 */
	public PortletControl(String titleBarText) {
		super(titleBarText);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param titleBarText
	 * @param titleIsVB
	 */
	public PortletControl(String titleBarText, boolean titleIsVB) {
		super(titleBarText, titleIsVB);
		// TODO Auto-generated constructor stub
	}
	
	
	public void encodeChildren(FacesContext context){
		
		IWContext iwc = IWContext.getIWContext(context);
		PortletContainer container;
		try {
			container = PortletUtils.getPortletContainer(iwc.getServletContext());

			initializePortlet(iwc,container);
			PortletDD portlet = getPortlet();
			renderPortlet(iwc, container, portlet);
		} catch (PortletException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (PortletContainerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void initializePortlet(IWContext context, PortletContainer container) {

		//Portlet portlet = getPortlet();
		
		if(getPortletName()==null){
			setPortletName("AboutPortlet");
		}
		if(getPortletAppId()==null){
			setPortletAppId("/localhost");
		}
		
		try {
			
			//Iterator iter = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplications();
			//Iterator iter = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplications();
			Iterator iter = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplicationIds();
			
			while (iter.hasNext()) {
				String id = (String)iter.next();
				//PortletContext app = (PortletContext) iter.next();
				
				System.out.println("PortletAppId: "+id);
				
				
				//container.getOptionalContainerServices().getPortletRegistryService().
				PortletAppDD appDD = container.getOptionalContainerServices().getPortletRegistryService().getPortletApplicationDescriptor(id);
//				System.out.println(app.getPortletContextName());
//				
				List portlets = appDD.getPortlets();
				for (Iterator iterator = portlets.iterator(); iterator
						.hasNext();) {
					Object oportlet = iterator.next();
					PortletDD portlet = (PortletDD)oportlet;
				
					if(portlet.getPortletName().equals(getPortletName())&&id.equals(getPortletAppId())){
						setPortlet(portlet);
						setMode(PortletMode.HELP);
					}
				}
			}
			
			
			/*
			
			//			portlet.init(container.getRequiredContainerServices().	
//		Iterator iterator = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplicationIds();
//		while (iterator.hasNext()) {
//			System.out.println(iterator.next());
//		}
//		
//		PortletWindow window = new PortletWindowImpl(new PortletWindowConfig(),new PortalURLImpl());

		try {
			//must xall processaction first
		//	portlet.processAction(new ActionRequestImpl(container,null,iwc.getRequest()), new ActionResponseImpl(container,null,iwc.getRequest(),iwc.getResponse()));
			//ortlet.render(rRequest,rResponse);
		}
		catch (PortletException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e) { 
			// TODO Auto-generated catch block
			e.printStackTrace();
		}*/
			
		} catch (PortletContainerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}

	private void renderPortlet(IWContext iwc, PortletContainer container,
			PortletDD portlet) throws PortletException, IOException,
			PortletContainerException {
		System.out.println("PortletName:"+portlet.getPortletName());
		System.out.println("PortletClass:"+portlet.getPortletClass());
		//if(portlet.getPortletName().equals("AboutPortlet")){

			HttpServletRequest request=iwc.getRequest();
			//request.getRequestDispatcher("asdf").
			HttpServletResponse realresponse=iwc.getResponse();
			HttpServletResponse response = new PortletJSFResponseWrapper(realresponse,iwc.getResponseWriter());
			
		    PortalRequestContext portalRequestContext =
		        new PortalRequestContext(iwc.getServletContext(), request, response);

		    PortalURL portalURL = portalRequestContext.getRequestedPortalURL();
		    String actionWindowId = portalURL.getActionWindow();

		    PortletWindowConfig actionWindowConfig =
		        actionWindowId == null
		            ? null
		            : PortletWindowConfig.fromId(actionWindowId);
		    
			
			PortletWindowConfig config = new PortletWindowConfig();
			config.setContextPath("/");
			config.setPortletName(portlet.getPortletName());
			String protocol="http";
			int port=iwc.getServerPort();
			String contextpath=iwc.getIWMainApplication().getApplicationContextURI();
			PortalURLParser urlParser = PortalURLParserImpl.getParser();
			String servletName="/plutoinvoker";
			String hostname=iwc.getServerName();
			PortalURL portalUrl=new PortalURLImpl(protocol,hostname,port,contextpath,servletName,urlParser);
			String windowId=portlet.getPortletName();
			portalURL.setWindowState(windowId, getState());
			portalURL.setPortletMode(windowId, getMode());
			PortletWindow window = new PortletWindowImpl(config,portalUrl);
			InternalPortletWindow internalPortletWindow = new InternalPortletWindowImpl(iwc.getServletContext(),window);
			boolean isAction=false;
			//PortletURL url = new PortletURLImpl(container,internalPortletWindow, iwc.getRequest(), iwc.getResponse(), isAction);
		
		

			//InternalPortletWindow internalPortletWindow;
			//RenderRequest rRequest = new RenderRequestImpl(container,internalPortletWindow,request);
			//RenderResponse rResponse = new RenderResponseImpl(container,internalPortletWindow,request, response);
			
			
			container.doRender(window, request, response);
		//}
	}

	/**
	 * @return Returns the portletApp.
	 */
	public String getPortletAppId() {
		return portletAppId;
	}

	/**
	 * @param portletApp The portletApp to set.
	 */
	public void setPortletAppId(String portletApp) {
		this.portletAppId = portletApp;
	}

	/**
	 * @return Returns the portletName.
	 */
	public String getPortletName() {
		return portletName;
	}

	/**
	 * @param portletName The portletName to set.
	 */
	public void setPortletName(String portletName) {
		this.portletName = portletName;
	}

	/**
	 * @return Returns the portlet.
	 */
	private PortletDD getPortlet() {
		return portlet;
	}

	/**
	 * @param portlet The portlet to set.
	 */
	private void setPortlet(PortletDD portlet) {
		this.portlet = portlet;
		setTitle(portlet.getPortletName());
	}

	/**
	 * @return Returns the state.
	 */
	public WindowState getState() {
		return state;
	}

	/**
	 * @param state The state to set.
	 */
	public void setState(WindowState state) {
		this.state = state;
	}

	/**
	 * @return Returns the mode.
	 */
	public PortletMode getMode() {
		return mode;
	}

	/**
	 * @param mode The mode to set.
	 */
	public void setMode(PortletMode mode) {
		this.mode = mode;
	}
	
}
