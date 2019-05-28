package com.idega.portal.service;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.oauth2.provider.ClientDetails;
import org.springframework.security.oauth2.provider.client.JdbcClientDetailsService;

import com.idega.core.business.DefaultSpringBean;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.FooterData;
import com.idega.portal.model.OAuthInfo;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.security.SecurityUtil;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.RequestResponseProvider;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.text.Name;

public class PortalSettingsResolver extends DefaultSpringBean {

	@Autowired(required=false)
	@Qualifier("clientDetails")
	private JdbcClientDetailsService clientDetailsService;

	@Autowired
	private LocalizationService localizationService;

	private LocalizationService getLocalizationService() {
		if (localizationService == null) {
			ELUtil.getInstance().autowire(this);
		}
		return localizationService;
	}

	private JdbcClientDetailsService getClientDetailsService() {
		if (this.clientDetailsService == null) {
			ELUtil.getInstance().autowire(this);
		}

		return clientDetailsService;
	}

	public PortalSettings getDashboardSettings(IWContext iwc) {
		User user = null;
		try {
			user = SecurityUtil.getInstance().getAuthorizedUser(iwc);
		} catch (Exception e) {}
		if (user == null) {
			user = getCurrentUser();
		}
		return getDashboardSettings(user, iwc);
	}

	public PortalSettings getDashboardSettings(User user, IWContext iwc) {
		PortalSettings settings = new PortalSettings();
		try {
			IWMainApplicationSettings appSettings = getSettings();
			String name = appSettings.getProperty("portal.name");
			settings.setName(name);

			Locale localeToUse = null;
			Locale defaultLocale = getApplication().getDefaultLocale();
			Locale currentLocale = getCurrentLocale();
			if (defaultLocale != null
					&& currentLocale != null
					&& !defaultLocale.toString().equals(
							currentLocale.toString())
					&& ICLocaleBusiness.isLocaleInUse(currentLocale.toString())) {
				localeToUse = currentLocale;
			}
			localeToUse = localeToUse == null ? defaultLocale : localeToUse;
			localeToUse = localeToUse == null ? Locale.ENGLISH : localeToUse;
			settings.setLocale(localeToUse.toString());
			settings.setLocalizations(getLocalizationService().getLocalizations(iwc.getRequest(), iwc.getResponse(), iwc.getServletContext()));

			settings.addCSSFile("/style/style.css");
			settings.setLogo("/images/main-logo.png");
			settings.setFavicon("/images/favicon.ico");

			settings.setOauthInfo(getOAuthSettings());

			settings.setAuthorizationSettings(SecurityUtil.getInstance().getAllAuthorizationSettings());

			settings.setMainPortalPage(appSettings.getProperty("portal.main_portal_page"));

			if (user != null) {
				settings.setLoggedIn(Boolean.TRUE);
				settings.setUser(new com.idega.block.oauth2.server.authentication.bean.User(user));

				settings.setMenus(getMenus(user));

				settings.setRoles(SecurityUtil.getInstance().getAllRoles(iwc, user));

				Name nameUtil = new Name(user.getName());
				settings.setFirstName(nameUtil.getFirstName());
				settings.setMiddleName(nameUtil.getMiddleName());
				settings.setLastName(nameUtil.getLastName());
			}

			FooterData footerData = getFooterData();
			if (footerData != null) {
				settings.setFooterData(footerData);
			}

			settings.setMainPortalLink(appSettings.getProperty("portal.main_portal_url"));
			settings.setMainPortalLabel(appSettings.getProperty("portal.main_portal_label"));

			Locale locale = getCurrentLocale();
			if (locale != null) {
				DateFormat dateFormatter = DateFormat.getDateInstance(DateFormat.SHORT, locale);
				if (dateFormatter instanceof SimpleDateFormat) {
					String datePattern = ((SimpleDateFormat) dateFormatter).toPattern();
					settings.setDatePattern(datePattern);
				}

				DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, locale);
				if (timeFormatter instanceof SimpleDateFormat) {
					String timePattern = ((SimpleDateFormat) timeFormatter).toPattern();
					settings.setTimePattern(timePattern);
				}
			}

			RequestResponseProvider rrProvider = null;
			try {
				rrProvider = ELUtil.getInstance().getBean(RequestResponseProvider.class);
			} catch (Exception e) {}
			if (rrProvider != null) {
				HttpServletRequest request = rrProvider.getRequest();
				if (request != null) {
					HttpSession session = null;
					try {
						session = request.getSession();
					} catch (Exception e) {}
					if (session != null) {
						settings.setSessionId(session.getId());
					}
				}
			}
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting settings", e);
		}
		return settings;
	}

	private FooterData getFooterData() {
		FooterData footerData = new FooterData();
		footerData.setMunicipality(getApplicationProperty(PortalConstants.PROPERTY_FOOTER_MUNICIPALITY, CoreConstants.EMPTY));
		footerData.setAddress(getApplicationProperty(PortalConstants.PROPERTY_FOOTER_ADDRESS, CoreConstants.EMPTY));
		footerData.setPhoneNumber(getApplicationProperty(PortalConstants.PROPERTY_FOOTER_PHONE_NUMBER, CoreConstants.EMPTY));
		footerData.setFaxNumber(getApplicationProperty(PortalConstants.PROPERTY_FOOTER_FAX_NUMBER, CoreConstants.EMPTY));
		footerData.setEmailAddress(getApplicationProperty(PortalConstants.PROPERTY_FOOTER_EMAIL_ADDRESS, CoreConstants.EMPTY));
		return footerData;
	}

	private List<PortalMenu> getMenus(User user) {
		List<PortalMenu> menus = new ArrayList<>();
		if (user == null) {
			getLogger().warning("User is unknown");
			return menus;
		}

		return menus;
	}

	private List<OAuthInfo> getOAuthSettings() {
		List<OAuthInfo> info = new ArrayList<>();
		try {
			List<ClientDetails> clientsDetails = getClientDetailsService().listClientDetails();
			if (ListUtil.isEmpty(clientsDetails)) {
				return info;
			}

			for (ClientDetails clientDetails: clientsDetails) {
				info.add(
						new OAuthInfo(
								clientDetails.getClientId(),
								clientDetails.getClientSecret(),
								StringUtil.getValue(clientDetails.getAuthorizedGrantTypes()),
								clientDetails.getAccessTokenValiditySeconds(),
								clientDetails.getRefreshTokenValiditySeconds()
						)
				);
			}
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting OAuth settings", e);
		}
		return info;
	}

}