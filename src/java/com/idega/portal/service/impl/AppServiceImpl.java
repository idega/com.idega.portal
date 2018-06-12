package com.idega.portal.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.business.DefaultSpringBean;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.service.AppService;
import com.idega.portal.service.PortalService;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Qualifier(PortalConstants.QUALIFIER_APP)
public class AppServiceImpl extends DefaultSpringBean implements AppService {

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_PORTAL)
	private PortalService portalService;

	@Override
	public PortalSettings getDashboardSettings() {
		return portalService.getDashboardSettings();
	}

	@Override
	public String logout(){
		return portalService.logout();
	}

}