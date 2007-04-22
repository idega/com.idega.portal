/*
 * $Id: IWBundleStarter.java,v 1.1 2007/04/22 14:59:25 eiki Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import org.apache.pluto.PortletContainerException;

import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.portal.util.PortletUtils;


/**
 * 
 *  Last modified: $Date: 2007/04/22 14:59:25 $ by $Author: eiki $
 * 
 * @author <a href="mailto:eiki@idega.com">Eirikur Hrafnsson</a>
 * @version $Revision: 1.1 $
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
