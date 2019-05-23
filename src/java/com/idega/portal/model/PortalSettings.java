package com.idega.portal.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.oauth2.server.authentication.bean.User;
import com.idega.block.sso.model.AuthorizationSettings;
import com.idega.core.location.data.bean.Location;
import com.idega.util.StringUtil;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class PortalSettings implements Serializable {

	private static final long serialVersionUID = -2750061914031027789L;

	private String name, firstName, middleName, lastName, logo, locale, favicon, mainPortalPage, sessionId;

	private List<String> cssFiles;

	private Map<String, String> colors, fonts;

	private Boolean loggedIn = Boolean.FALSE;

	private User user;

	private List<PortalMenu> menus;

	private List<AuthorizationSettings> authorizationSettings;

	private FooterData footerData;

	private Map<String, LanguageData> localizations;

	private List<String> roles;

	private String mainPortalLink, mainPortalLabel;

	private List<OAuthInfo> oauthInfo;

	private String datePattern, timePattern;

	private Location location;

	private Boolean useLDAP = Boolean.FALSE;

	private List<String> systemRoles = new ArrayList<String>();

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLogo() {
		return logo;
	}

	public void setLogo(String logo) {
		this.logo = logo;
	}

	public String getLocale() {
		return locale;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}

	public Map<String, String> getColors() {
		return colors;
	}

	public void setColors(Map<String, String> colors) {
		this.colors = colors;
	}

	public Map<String, String> getFonts() {
		return fonts;
	}

	public void setFonts(Map<String, String> fonts) {
		this.fonts = fonts;
	}

	public void addColor(String name, String value) {
		addProperty(name, value, colors);
	}

	public void addFont(String name, String value) {
		addProperty(name, value, fonts);
	}

	private void addProperty(String name, String value, Map<String, String> properties) {
		if (StringUtil.isEmpty(value) || StringUtil.isEmpty(name)) {
			return;
		}

		if (properties == null) {
			properties = new HashMap<>();
		}

		properties.put(name, value);
	}

	public void addCSSFile(String cssFile) {
		if (StringUtil.isEmpty(cssFile)) {
			return;
		}

		if (cssFiles == null) {
			cssFiles = new ArrayList<>();
		}

		cssFiles.add(cssFile);
	}

	public Boolean getLoggedIn() {
		return loggedIn;
	}

	public void setLoggedIn(Boolean loggedIn) {
		this.loggedIn = loggedIn;
	}

	public User getUser() {
		return user;
	}

	public void setUser(User user) {
		this.user = user;
	}

	public List<PortalMenu> getMenus() {
		return menus;
	}

	public void setMenus(List<PortalMenu> menus) {
		this.menus = menus;
	}

	public String getFavicon() {
		return favicon;
	}

	public void setFavicon(String favicon) {
		this.favicon = favicon;
	}

	public String getMainPortalPage() {
		return mainPortalPage;
	}

	public void setMainPortalPage(String mainPortalPage) {
		this.mainPortalPage = mainPortalPage;
	}

	public FooterData getFooterData() {
		return footerData;
	}

	public void setFooterData(FooterData footerData) {
		this.footerData = footerData;
	}

	public Map<String, LanguageData> getLocalizations() {
		return localizations;
	}

	public void setLocalizations(Map<String, LanguageData> localizations) {
		this.localizations = localizations;
	}

	public List<String> getRoles() {
		return roles;
	}

	public void setRoles(List<String> roles) {
		this.roles = roles;
	}

	public List<AuthorizationSettings> getAuthorizationSettings() {
		return authorizationSettings;
	}

	public void setAuthorizationSettings(List<AuthorizationSettings> authorizationSettings) {
		this.authorizationSettings = authorizationSettings;
	}

	public void addAuthorizationSettings(AuthorizationSettings settings) {
		if (settings == null) {
			return;
		}

		if (authorizationSettings == null) {
			authorizationSettings = new ArrayList<>();
		}

		authorizationSettings.add(settings);
	}

	public String getMainPortalLink() {
		return mainPortalLink;
	}

	public void setMainPortalLink(String mainPortalLink) {
		this.mainPortalLink = mainPortalLink;
	}

	public String getMainPortalLabel() {
		return mainPortalLabel;
	}

	public void setMainPortalLabel(String mainPortalLabel) {
		this.mainPortalLabel = mainPortalLabel;
	}

	public List<OAuthInfo> getOauthInfo() {
		return oauthInfo;
	}

	public void setOauthInfo(List<OAuthInfo> oauthInfo) {
		this.oauthInfo = oauthInfo;
	}

	public List<String> getCssFiles() {
		return cssFiles;
	}

	public void setCssFiles(List<String> cssFiles) {
		this.cssFiles = cssFiles;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getDatePattern() {
		return datePattern;
	}

	public void setDatePattern(String datePattern) {
		this.datePattern = datePattern;
	}

	public String getTimePattern() {
		return timePattern;
	}

	public void setTimePattern(String timePattern) {
		this.timePattern = timePattern;
	}

	public final Location getLocation() {
		return location;
	}

	public final void setLocation(Location location) {
		this.location = location;
	}

	public Boolean getUseLDAP() {
		return useLDAP;
	}

	public void setUseLDAP(Boolean useLDAP) {
		this.useLDAP = useLDAP;
	}

	public String getSessionId() {
		return sessionId;
	}

	public void setSessionId(String sessionId) {
		this.sessionId = sessionId;
	}
	
	public List<String> getSystemRoles() {
		return systemRoles;
	}

}