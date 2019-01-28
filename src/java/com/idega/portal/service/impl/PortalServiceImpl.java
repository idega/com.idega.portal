package com.idega.portal.service.impl;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response.Status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.security.oauth2.provider.ClientDetails;
import org.springframework.security.oauth2.provider.client.JdbcClientDetailsService;
import org.springframework.stereotype.Service;

import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.data.Email;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.FooterData;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.OAuthInfo;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.LocalizationService;
import com.idega.portal.service.PortalService;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.WebUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.text.Name;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Qualifier(PortalConstants.QUALIFIER_PORTAL)
public class PortalServiceImpl extends DefaultSpringBean implements PortalService {

	@Autowired
	private WebUtil webUtil;

	@Autowired(required=false)
	@Qualifier("clientDetails")
	private JdbcClientDetailsService clientDetailsService;

	@Autowired
	private LocalizationService localizationService;

	private JdbcClientDetailsService getClientDetailsService() {
		if (this.clientDetailsService == null) {
			ELUtil.getInstance().autowire(this);
		}

		return clientDetailsService;
	}

	@Override
	public PortalSettings getDashboardSettings() {
		PortalSettings settings = new PortalSettings();
		try {
			IWMainApplicationSettings appSettings = getSettings();
			String name = appSettings.getProperty("portal.name");
			settings.setName(name);

			Locale localeToUse = null;
			Locale defaultLocale = getApplication().getDefaultLocale();
			Locale currentLocale = getCurrentLocale();
			if (defaultLocale != null && currentLocale != null && !defaultLocale.toString().equals(currentLocale.toString())) {
				localeToUse = currentLocale;
			}
			localeToUse = localeToUse == null ? defaultLocale : localeToUse;
			localeToUse = localeToUse == null ? Locale.ENGLISH : localeToUse;
			settings.setLocale(localeToUse.toString());
			settings.setLocalizations(localizationService.getLocalizations());

			settings.addCSSFile("/style/style.css");
			settings.setLogo("/images/main-logo.png");
			settings.setFavicon("/images/favicon.ico");

			settings.setOauthInfo(getOAuthSettings());

			settings.setAuthorizationSettings(SecurityUtil.getInstance().getAllAuthorizationSettings());

			settings.setMainPortalPage(appSettings.getProperty("portal.main_portal_page"));

			User user = null;
			try {
				user = SecurityUtil.getInstance().getAuthorizedUser();
			} catch (Exception e) {}
			if (user == null) {
				user = getCurrentUser();
			}
			if (user != null) {
				settings.setLoggedIn(Boolean.TRUE);
				settings.setUser(new com.idega.block.oauth2.server.authentication.bean.User(user));

				settings.setMenus(getMenus(user));

				settings.setRoles(SecurityUtil.getInstance().getAllRoles(user));

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

	private UserBusiness getUserBusiness() {
		return getServiceInstance(UserBusiness.class);
	}

	@Override
	public List<PortalMenu> getPortalMenus() {
		User user = null;
		try {
			user = SecurityUtil.getInstance().getAuthorizedUser();
			return getMenus(user);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting menu for " + user, e);
		}
		return null;
	}

	private List<PortalMenu> getMenus(User user) {
		List<PortalMenu> menus = new ArrayList<>();
		if (user == null) {
			getLogger().warning("User is unknown");
			return menus;
		}

		return menus;
	}

	@Override
	public UserAccount doCreateAccount(UserAccount account) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));

		if (account == null) {
			account = new UserAccount();
			account.setErrorMessage(iwrb.getLocalizedString("account.account_data_not_provided", "Please provide data for account"));
			return account;
		}

		if (StringUtil.isEmpty(account.getUsername()) || StringUtil.isEmpty(account.getPassword())) {
			account.setErrorMessage(iwrb.getLocalizedString("account.provide_credentials", "Please provide credentials for account"));
			return account;
		}

		try {
			LoginTable login = LoginDBHandler.getUserLoginByUserName(account.getUsername());
			if (login != null) {
				account.setErrorMessage(iwrb.getLocalizedString("account.please_choose_different_user_name", "Please choose different username"));
				return account;
			}

			com.idega.user.data.User user = null;

			String firstName = null;
			String middleName = null;
			String lastName = null;
			String displayName = null;

			if (!StringUtil.isEmpty(account.getName())) {
				Name userNames = new Name(account.getName());
				firstName = userNames.getFirstName();
				middleName = userNames.getMiddleName();
				lastName = userNames.getLastName();
				displayName = account.getName();
			}

			if (!getUserBusiness().validatePersonalId(account.getPersonalId(), getCurrentLocale())) {
				account.setErrorMessage(iwrb.getLocalizedString("account.provide_valid_personal_id", "Please provide valid personal ID"));
				return account;
			}

			if (!StringUtil.isEmpty(account.getUuid())) {
				user = getUserBusiness().update(
						null,
						account.getUuid(),
						null,
						firstName,
						middleName,
						lastName,
						displayName,
						account.getPersonalId(),
						account.getEmail(),
						null,
						account.getUsername(),
						account.getPassword());
			} else {
				user = getUserBusiness().createUserWithLogin(
						firstName,
						middleName,
						lastName,
						account.getPersonalId(),
						displayName,
						null,
						null,
						null,
						null,
						account.getUsername(),
						account.getPassword(),
						Boolean.TRUE,
						IWTimestamp.RightNow(),
						5000,
						Boolean.FALSE,
						Boolean.TRUE,
						Boolean.FALSE,
						null);
			}

			if (user == null) {
				account.setErrorMessage(iwrb.getLocalizedString("account.failed_to_create_account", "Sorry, some error occurred - failed to create account. Please try later"));
				return account;
			}

			if (!StringUtil.isEmpty(account.getEmail())) {
				Email email = getUserBusiness().updateUserMail(user, account.getEmail());
				if (email == null) {
					getLogger().log(
							Level.WARNING,
							"Error updating email: " + account.getEmail()
									+ " for user (ID: "
									+ user.getPrimaryKey().toString() + ")");
				}
			}

			account.setUserId(user.getPrimaryKey().toString());
			account.setErrorMessage(null);
		} catch (Exception e) {
			account.setErrorMessage(iwrb.getLocalizedString("account.failed_to_create_account", "Sorry, some error occurred - failed to create account. Please try later"));
			getLogger().log(Level.WARNING, "Error creating account for " + account.getUsername(), e);
		}

		return account;
	}

	@Override
	public String doAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String type) {
		getLogger().warning("This method is not implemented");
		return null;
	}

	@Override
	public String doUnAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String uri) {
		getLogger().warning("This method is not implemented");
		return null;
	}

	@Override
	public String setLanguage(String language) {
		return localizationService.setLanguage(language);
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

	@Override
	public String logout() {
		return webUtil.logOut();
	}

	@Override
	public String doRemindPassword(String ssn) {
		return "Unimplemented";
	}

	@Override
	public Result setLocalization(Localization localization) {
		return localizationService.setLocalization(localization);
	}

	@Override
	public Result setLocalizations(Localizations localizations) {
		return localizationService.setLocalizations(localizations);
	}

	@Override
	public List<LanguageData> getAvailableLanguages() {
		return localizationService.getAvailableLanguages();
	}

	@Override
	public Result addLanguage(String locale) {
		return localizationService.addLanguage(locale);
	}

	@Override
	public Result removeLanguage(String locale) {
		return localizationService.removeLanguage(locale);
	}

	@Override
	public Result doPing() {
		try {
			return new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
		} catch (Exception e) {}
		return new Result(Status.INTERNAL_SERVER_ERROR.getStatusCode(), Boolean.FALSE.toString());
	}

}