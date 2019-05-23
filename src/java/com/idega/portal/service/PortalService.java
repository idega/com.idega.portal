package com.idega.portal.service;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;

public interface PortalService {

	public PortalSettings getDashboardSettings();

	public List<PortalMenu> getPortalMenus();

	public UserAccount doCreateAccount(UserAccount account);

	public String doAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String type);

	public String doUnAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String uri);

	public String setLanguage(String language);

	public String logout();

	public String doRemindPassword(String ssn);

	public Result setLocalization(Localization localization);

	public Result setLocalizations(Localizations localizations);

	public List<LanguageData> getAvailableLanguages();

	public Result addLanguage(String locale);

	public Result removeLanguage(String locale);

	public Result doPing();

}