package com.idega.portal.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.UserAccount;

public interface PortalService {

	public PortalSettings getDashboardSettings();

	public List<PortalMenu> getPortalMenus();

	public UserAccount doCreateAccount(UserAccount account);

	public String doAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String type);

	public String setLanguage(String language);

}