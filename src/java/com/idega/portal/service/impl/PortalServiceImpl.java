package com.idega.portal.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.core.localisation.data.ICLocale;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.FooterData;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.OAuthInfo;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.UserAccount;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.PortalService;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.LocaleUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.datastructures.map.MapUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.messages.MessageResource;
import com.idega.util.messages.MessageResourceFactory;
import com.idega.util.text.Name;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Qualifier(PortalConstants.QUALIFIER_PORTAL)
public class PortalServiceImpl extends DefaultSpringBean implements PortalService {

	@Autowired
	private MessageResourceFactory messageResourceFactory;

	private MessageResourceFactory getMessageResourceFactory() {
		if (this.messageResourceFactory == null) {
			this.messageResourceFactory = ELUtil.getInstance().getBean(MessageResourceFactory.class);
		}
		return this.messageResourceFactory;
	}

	@Autowired(required=false)
	@Qualifier("clientDetails")
	private JdbcClientDetailsService clientDetailsService;

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
			settings.setLocalizations(getLocalizations());

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

	private Map<String, LanguageData> getLocalizations() {
		Map<String, LanguageData> localizations = new HashMap<String, LanguageData>();

		String bundleIdentifier = getApplicationProperty(PortalConstants.PROPERTY_PORTAL_LOCALIZER_BUNDLE_ID, PortalConstants.IW_BUNDLE_IDENTIFIER);

		List<ICLocale> icLocales = ICLocaleBusiness.listOfLocales(true);
		if (!ListUtil.isEmpty(icLocales)) {
			for (ICLocale icLocale : icLocales) {
				LanguageData data = getLocalizedStrings(
						LocaleUtil.getLocale(icLocale.toString()),
						bundleIdentifier
				);
				if (data != null) {
					localizations.put(icLocale.toString(), data);
				}
			}
		}

		return localizations;
	}

	private LanguageData getLocalizedStrings(Locale locale, String bundleIdentifier) {
		List<MessageResource> resourceList;
		resourceList = getMessageResourceFactory().getResourceListByBundleAndLocale(bundleIdentifier, locale);

		LanguageData languageData = new LanguageData();
		languageData.setLocale(locale.toString());
		languageData.setLanguage(StringHandler.firstCharacterToUpperCaseRestToLowerCase(locale.getDisplayLanguage(locale)));
		languageData.setLocalizedStrings(new HashMap<String, String>());

		Map<String, Map<MessageResource, String>> localizedStrings = getLocalizedStrings(resourceList);
		for (String key: localizedStrings.keySet()) {
			Map<MessageResource, String> valueMap = localizedStrings.get(key);
			if (!MapUtil.isEmpty(valueMap)) {
				List<MessageResource> resources = new ArrayList<>(valueMap.keySet());
				Collections.sort(resources, new Comparator<MessageResource>() {

					@Override
					public int compare(MessageResource o1, MessageResource o2) {
						if (o1.getLevel().intValue() > o2.getLevel().intValue()) {
							return -1;
						}

						if (o2.getLevel().intValue() > o1.getLevel().intValue()) {
							return 1;
						}

						return 0;
					}
				});

				String localizedString = CoreConstants.EMPTY;

				for (MessageResource resource : resources) {
					localizedString = valueMap.get(resource) == null ? localizedString :  valueMap.get(resource);
					if (!CoreConstants.EMPTY.equals(localizedString)){
						break;
					}
				}
				languageData.getLocalizedStrings().put(key, localizedString);
			}
		}

		return languageData;
	}

	private Map<String, Map<MessageResource, String>> getLocalizedStrings(List<MessageResource> resources) {
		Map<String, Map<MessageResource, String>> localizedStrings = new TreeMap<>();

		if (!ListUtil.isEmpty(resources)) {
			for (MessageResource resource : resources) {
				Set<String> keys = resource.getAllLocalizedKeys();
				if (!ListUtil.isEmpty(keys)) {
					for (String key : keys) {
						String value = resource.getMessage(key);
						if (!StringUtil.isEmpty(value)) {
							Map<MessageResource, String> valueMap = localizedStrings.get(key);
							if (MapUtil.isEmpty(valueMap)) {
								valueMap = new HashMap<>();
								localizedStrings.put(key, valueMap);
							}

							valueMap.put(resource, value);
						}
					}
				}
			}
		}

		return localizedStrings;
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

			user = getUserBusiness().createUserWithLogin(firstName, middleName,
					lastName, account.getPersonalId(), displayName, null, null,
					null, null, account.getUsername(), account.getPassword(),
					Boolean.TRUE, IWTimestamp.RightNow(), 5000, Boolean.FALSE,
					Boolean.TRUE, Boolean.FALSE, null);
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
	public String setLanguage(String language) {
		if (StringUtil.isEmpty(language)) {
			return null;
		}

		try {
			Locale locale = ICLocaleBusiness.getLocaleFromLocaleString(language);
			if (locale == null) {
				return null;
			}

			IWContext iwc = CoreUtil.getIWContext();
			iwc.setCurrentLocale(locale);
			return language;
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error setting language " + language, e);
		}

		return null;
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