package com.idega.portal.service;

import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;

public interface AppService {

	public PortalSettings getDashboardSettings();

	public String logout();

	public Result isValidLogin(UserAccount credentials);

}