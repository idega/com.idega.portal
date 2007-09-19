/*
 * $Id: PortletControl.java,v 1.3 2007/09/19 10:34:48 eiki Exp $
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

import javax.faces.context.FacesContext;
import javax.portlet.Portlet;
import javax.portlet.PortletContext;
import javax.portlet.PortletException;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;

import org.apache.pluto.PortletContainer;
import org.apache.pluto.PortletContainerException;

import com.idega.portal.util.PortletRequestWrapper;
import com.idega.portal.util.PortletResponseWrapper;
import com.idega.portal.util.PortletUtils;
import com.idega.presentation.IWContext;
import com.idega.webface.WFBlock;


/**
 * <p>
 * JSF Component to control a wrapped Portlet instance.
 * </p>
 *  Last modified: $Date: 2007/09/19 10:34:48 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.3 $
 */
public class PortletControl extends WFBlock {

	private transient Portlet portlet;

	/**
	 * 
	 */
	public PortletControl() {
		super();
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
	
	public Portlet getPortlet(){
		if(portlet==null){
			portlet = new HelloWorld();
//			portlet = new FacesComponentPortlet(new Text("my test portlet"));
		}
		return portlet;
	}
	
	
	public void encodeChildren(FacesContext context){
		
		IWContext iwc = IWContext.getIWContext(context);
		
	
		try {
			PortletContainer container = PortletUtils.getPortletContainer(iwc.getServletContext());
			
			Iterator iter = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplications();
			while (iter.hasNext()) {
				PortletContext app = (PortletContext) iter.next();
				
				
				System.out.println(app.getPortletContextName());
				
				//container.getOptionalContainerServices().getPortletRegistryService().
				//PortletAppDD appDD = container.getOptionalContainerServices().getPortletRegistryService().getPortletApplicationDescriptor(app.getPortletContextName());
//				System.out.println(app.getPortletContextName());
//				
//				List portlets = appDD.getPortlets();
//				for (Iterator iterator = portlets.iterator(); iterator
//						.hasNext();) {
//					Portlet port = (Portlet) iterator.next();
//					System.out.println(port.getClass());
//				}
			}
			
			
			
			
			//			portlet.init(container.getRequiredContainerServices().
	
			
//		Iterator iterator = container.getOptionalContainerServices().getPortletRegistryService().getRegisteredPortletApplicationIds();
//		while (iterator.hasNext()) {
//			System.out.println(iterator.next());
//		}
//		
			
//		PortletWindow window = new PortletWindowImpl(new PortletWindowConfig(),new PortalURLImpl());
		
		Portlet portlet = getPortlet();

		RenderRequest rRequest = new PortletRequestWrapper(context,portlet);
		RenderResponse rResponse = new PortletResponseWrapper(context);
		
		try {
			//must xall processaction first
		//	portlet.processAction(new ActionRequestImpl(container,null,iwc.getRequest()), new ActionResponseImpl(container,null,iwc.getRequest(),iwc.getResponse()));
			portlet.render(rRequest,rResponse);
		}
		catch (PortletException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e) { 
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		} catch (PortletContainerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
	}
	
}
