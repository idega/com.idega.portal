/**
 * $Id: IWRenderConfigService.java,v 1.2 2007/04/20 23:31:10 eiki Exp $
 * Created in 2006 by tryggvil
 * 
 * Copyright (C) 2000-2006 Idega Software hf. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega hf. Use is subject to
 * license terms.
 */
package com.idega.portal.pluto;

import java.io.InputStream;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletContext;

import org.apache.pluto.driver.config.DriverConfigurationException;
import org.apache.pluto.driver.services.impl.resource.RenderConfigServiceImpl;
import org.apache.pluto.driver.services.impl.resource.ResourceConfig;
import org.apache.pluto.driver.services.impl.resource.ResourceConfigReader;
import org.apache.pluto.driver.services.portal.PageConfig;
import org.apache.pluto.driver.services.portal.RenderConfigService;
import org.apache.pluto.driver.services.portal.admin.RenderConfigAdminService;

import com.idega.portal.pluto.util.ConfigUtil;

/**
 * <p>
 * TODO tryggvil Describe Type IWRenderConfigService
 * </p>
 * Last modified: $Date: 2007/04/20 23:31:10 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class IWRenderConfigService extends RenderConfigServiceImpl implements RenderConfigService,
		RenderConfigAdminService {

	private ResourceConfig config;

	//
	// Lifecycle Methods
	//
	/**
	 * 
	 * Initialization Lifecycle Method
	 * 
	 * @param ctx
	 * 
	 */
	public void init(ServletContext ctx) {
		try {
			InputStream in = ConfigUtil.getConfigFileInputStream();
			config = ResourceConfigReader.getFactory().parse(in);
		}
		catch (Exception e) {
			throw new DriverConfigurationException(e);
		}
	}

	/**
	 * 
	 * Shutdown the ResourceService.
	 * 
	 */
	public void destroy() {
		config = null;
	}

	public String getPortalName() {
		return config.getPortalName();
	}

	public String getPortalVersion() {
		return config.getPortalVersion();
	}

	public String getContainerName() {
		return config.getContainerName();
	}

	public Set getSupportedPortletModes() {
		return config.getSupportedPortletModes();
	}

	public Set getSupportedWindowStates() {
		return config.getSupportedWindowStates();
	}

	public List getPages() {
		return config.getRenderConfig().getPages();
	}

	public PageConfig getDefaultPage() {
		return config.getRenderConfig().getPageConfig(null);
	}

	public PageConfig getPage(String id) {
		return config.getRenderConfig().getPageConfig(id);
	}

	public void addPage(PageConfig pageConfig) {
		config.getRenderConfig().addPage(pageConfig);
	}
}
