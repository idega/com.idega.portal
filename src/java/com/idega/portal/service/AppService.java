package com.idega.portal.service;

import com.idega.portal.model.PortalSettings;

public interface AppService {

	public PortalSettings getDashboardSettings();

	public String logout();

}