package com.idega.portal.service;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.model.UserProfile;

public interface AppService {

	public PortalSettings getDashboardSettings(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public String logout(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result isValidLogin(UserAccount credentials);

	public UserProfile getCitizenProfile(String personalId, HttpServletRequest request, HttpServletResponse response, ServletContext context);

}