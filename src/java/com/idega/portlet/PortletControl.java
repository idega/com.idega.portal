/*
 * $Id: PortletControl.java,v 1.1 2006/10/05 17:52:26 tryggvil Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portlet;

import java.io.IOException;
import javax.faces.context.FacesContext;
import javax.portlet.Portlet;
import javax.portlet.PortletException;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;
import com.idega.presentation.text.Text;
import com.idega.webface.WFBlock;


/**
 * <p>
 * JSF Component to control a wrapped Portlet instance.
 * </p>
 *  Last modified: $Date: 2006/10/05 17:52:26 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
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
			portlet = new FacesComponentPortlet(new Text("my test portlet"));
		}
		return portlet;
	}
	
	
	public void encodeChildren(FacesContext context){
		
		Portlet portlet = getPortlet();
		
		RenderRequest rRequest = null;
		RenderResponse rResponse = null;
		
		/*PortletRenderInfo info = PortletUtils.createRenderInfo(context);
		rRequest = info.getRequest();
		rResponse = info.getResponse();
		*/
		try {
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
		
	}
	
}
