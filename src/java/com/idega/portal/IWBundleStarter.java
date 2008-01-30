/*
 * $Id: IWBundleStarter.java,v 1.2 2008/01/30 14:32:04 tryggvil Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import org.apache.pluto.PortletContainerException;

import com.idega.core.view.ViewManager;
import com.idega.core.view.ViewNode;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.portal.util.PortletUtils;
import com.idega.workspace.view.WorkspaceClassViewNode;


/**
 * 
 *  Last modified: $Date: 2008/01/30 14:32:04 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:eiki@idega.com">Eirikur Hrafnsson</a>
 * @version $Revision: 1.2 $
 */
public class IWBundleStarter implements IWBundleStartable {
	public static final String BUNDLE_IDENTIFIER ="com.idega.portal";

	/**
	 * 
	 */
	public IWBundleStarter() {
	}

	/* (non-Javadoc)
	 * @see com.idega.idegaweb.IWBundleStartable#start(com.idega.idegaweb.IWBundle)
	 */
	public void start(IWBundle starterBundle) {
		//initialize it
		try {
			

			PortletUtils.getPortletContainer(starterBundle.getApplication().getServletContext());
			
			ViewManager viewManager = ViewManager.getInstance(starterBundle.getApplication());
			ViewNode workspace = viewManager.getWorkspaceRoot();
			
			WorkspaceClassViewNode classNode = new WorkspaceClassViewNode("portal",workspace);
			classNode.setComponentClass(PortletControl.class);
			
			
		} catch (PortletContainerException e) {
			e.printStackTrace();
		}
		
	}

	/* (non-Javadoc)
	 * @see com.idega.idegaweb.IWBundleStartable#stop(com.idega.idegaweb.IWBundle)
	 */
	public void stop(IWBundle starterBundle) {
		try {
			PortletUtils.getPortletContainer(starterBundle.getApplication().getServletContext()).destroy();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
