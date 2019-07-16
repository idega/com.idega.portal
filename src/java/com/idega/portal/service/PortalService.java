package com.idega.portal.service;

import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.Article;
import com.idega.portal.model.ArticleList;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.LoginResult;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;

public interface PortalService {

	public PortalSettings getDashboardSettings(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public List<PortalMenu> getPortalMenus(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public UserAccount doCreateAccount(UserAccount account, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public String doAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String type);

	public String doUnAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String uri);

	public String setLanguage(String language, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public String logout(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public String doRemindPassword(String ssn, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setLocalization(Localization localization, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setLocalizations(Localizations localizations, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public List<LanguageData> getAvailableLanguages(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result addLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result removeLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result doPing(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Article getArticleByURI(String uri, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public ArticleList getArticlesByCategory(String category, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public LoginResult login(String clientId, String username, String password, HttpServletRequest request, HttpServletResponse response, ServletContext context);

}