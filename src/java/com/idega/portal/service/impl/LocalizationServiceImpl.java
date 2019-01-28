package com.idega.portal.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;

import javax.ws.rs.core.Response.Status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.core.localisation.data.ICLocale;
import com.idega.development.business.LocalizerBusiness;
import com.idega.development.event.LocalizationChangedEvent;
import com.idega.idegaweb.RepositoryStartedEvent;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.Result;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.LocalizationService;
import com.idega.presentation.IWContext;
import com.idega.restful.business.DefaultRestfulService;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.LocaleUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.datastructures.map.MapUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.messages.MessageResource;
import com.idega.util.messages.MessageResourceFactory;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class LocalizationServiceImpl extends DefaultRestfulService implements LocalizationService, ApplicationListener<ApplicationEvent> {

	@Autowired
	private MessageResourceFactory messageResourceFactory;

	@Autowired
	private LocalizerBusiness localizerBusiness;

	private Map<String, LanguageData> localizations = null;

	private MessageResourceFactory getMessageResourceFactory() {
		if (this.messageResourceFactory == null) {
			this.messageResourceFactory = ELUtil.getInstance().getBean(MessageResourceFactory.class);
		}
		return this.messageResourceFactory;
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

	@Override
	public Result setLocalization(Localization localization) {
		if (localization == null || StringUtil.isEmpty(localization.getLocalizedKey()) || StringUtil.isEmpty(localization.getLocalization()) || StringUtil.isEmpty(localization.getBundleIdentifier())) {
			return null;
		}

		try {
			User user = SecurityUtil.getInstance().getAuthorizedUser();
			if (user == null) {
				return null;
			}

			String locale = localization.getLocale();
			locale = StringUtil.isEmpty(locale) ? getCurrentLocale().toString() : locale;

			localizerBusiness.storeLocalizedStrings(null, localization.getLocalizedKey(), localization.getLocalization(), localization.getBundleIdentifier(), locale, null);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error setting localization " + localization, e);
			return null;
		}

		return new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
	}

	@Override
	public Result setLocalizations(Localizations localizations) {
		if (localizations == null) {
			return null;
		}

		if (ListUtil.isEmpty(localizations.getLocalizations())) {
			return null;
		}

		User user = SecurityUtil.getInstance().getAuthorizedUser();
		if (user == null) {
			return null;
		}

		String bundleIdentifiersProp = getApplicationProperty(
				PortalConstants.PROPERTY_PORTAL_LOCALIZER_BUNDLE_ID,
				PortalConstants.IW_BUNDLE_IDENTIFIER);
		List<String> bundleIdentifiers = Arrays.asList(bundleIdentifiersProp
				.split(CoreConstants.COMMA));

		for (Localization localization : localizations.getLocalizations()) {
			try {
				if (StringUtil.isEmpty(localization.getBundleIdentifier())) {
					String bundle = getLocalizationBundle(localization,
							bundleIdentifiers);
					if (StringUtil.isEmpty(bundle)) {
						return null;
					}

					localization.setBundleIdentifier(bundle);
				}

				Result result = setLocalization(localization);
				if (result == null) {
					return null;
				}

			} catch (Exception e) {
				getLogger().log(Level.WARNING,
						"Error setting localization " + localization, e);
			}
		}

		return new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
	}

	@Override
	public List<LanguageData> getAvailableLanguages() {
		List<ICLocale> locales = ICLocaleBusiness.listOfLocales(false);
		if (ListUtil.isEmpty(locales)) {
			return null;
		}

		User user = SecurityUtil.getInstance().getAuthorizedUser();
		if (user == null) {
			return null;
		}

		List<LanguageData> availableLanguages = new ArrayList<LanguageData>();

		for (ICLocale icLocale : locales) {
			Locale locale = LocaleUtil.getLocale(icLocale.toString());
			if (locale != null) {
				availableLanguages.add(
						new LanguageData(
								locale.toString(),
								StringHandler.firstCharacterToUpperCaseRestToLowerCase(locale.getDisplayLanguage(locale)),
								StringHandler.firstCharacterToUpperCaseRestToLowerCase(locale.getDisplayCountry(locale))
						)
				);
			}
		}

		return availableLanguages;
	}

	@Override
	public Result addLanguage(String locale) {
		return addOrRemoveLanguage(locale, true) ? new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString()) : null;
	}

	@Override
	public Result removeLanguage(String locale) {
		return addOrRemoveLanguage(locale, false) ? new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString()) : null;
	}

	private String getLocalizationBundle(Localization localization,
			List<String> bundleIdentifiers) {
		if (localization == null || ListUtil.isEmpty(bundleIdentifiers)) {
			return null;
		}

		String key = localization.getLocalizedKey();
		Locale locale = StringUtil.isEmpty(localization.getLocale()) ? null
				: LocaleUtil.getLocale(localization.getLocale());
		if (StringUtil.isEmpty(key) || locale == null) {
			return null;
		}

		String bundle = null;
		String localizedString = null;

		for (String bundleIdentifier : bundleIdentifiers) {
			localizedString = getApplication().getMessageFactory()
					.getLocalizedMessage(key, null, bundleIdentifier, locale);

			if (localizedString != null) {
				bundle = bundleIdentifier;
			}
		}

		return bundle;
	}

	private boolean addOrRemoveLanguage(String locale, boolean doAdd) {
		if (StringUtil.isEmpty(locale)) {
			return false;
		}

		try {
			User user = SecurityUtil.getInstance().getAuthorizedUser();
			if (user == null) {
				return false;
			}

			ICLocale icLocale = ICLocaleBusiness.getICLocale(locale);
			if (icLocale == null) {
				return false;
			}

			ICLocaleBusiness.makeLocaleInUse(icLocale.getPrimaryKey()
					.toString(), doAdd);

			this.localizations = null;

			return true;
		} catch (Exception e) {
			getLogger().log(
					Level.WARNING,
					"Error " + (doAdd ? "adding" : "removing") + " language: "
							+ locale, e);
		}

		return false;
	}

	@Override
	public Map<String, LanguageData> getLocalizations() {
		if (localizations != null) {
			return new HashMap<String, LanguageData>(localizations);
		}

		Map<String, LanguageData> localizations = new HashMap<String, LanguageData>();

		String bundleIdentifiersProp = getApplicationProperty(PortalConstants.PROPERTY_PORTAL_LOCALIZER_BUNDLE_ID, PortalConstants.IW_BUNDLE_IDENTIFIER);
		List<String> bundleIdentifiers = Arrays.asList(bundleIdentifiersProp.split(CoreConstants.COMMA));

		List<ICLocale> icLocales = ICLocaleBusiness.listOfLocales(true);
		if (ListUtil.isEmpty(icLocales)) {
			this.localizations = localizations;
			return localizations;
		}

		for (ICLocale icLocale: icLocales) {
			LanguageData data = getLocalizedStrings(
					LocaleUtil.getLocale(icLocale.toString()),
					bundleIdentifiers
			);
			if (data != null) {
				localizations.put(icLocale.toString(), data);
			}
		}

		this.localizations = localizations;
		return localizations;
	}

	private LanguageData getLocalizedStrings(Locale locale, List<String> bundleIdentifiers) {
		LanguageData languageData = new LanguageData();
		languageData.setLocale(locale.toString());
		languageData.setLanguage(StringHandler.firstCharacterToUpperCaseRestToLowerCase(locale.getDisplayLanguage(locale)));
		languageData.setLocalizedStrings(new HashMap<String, String>());

		if (ListUtil.isEmpty(bundleIdentifiers)) {
			return languageData;
		}

		for (String bundleIdentifier: bundleIdentifiers) {
			if (bundleIdentifier == null) {
				continue;
			}

			bundleIdentifier = bundleIdentifier.trim();
			if (StringUtil.isEmpty(bundleIdentifier)) {
				continue;
			}

			List<MessageResource> resourceList = getMessageResourceFactory().getResourceListByBundleAndLocale(bundleIdentifier, locale);

			Map<String, Map<MessageResource, String>> localizedStrings = getLocalizedStrings(resourceList);
			for (String key: localizedStrings.keySet()) {
				Map<MessageResource, String> valueMap = localizedStrings.get(key);
				if (MapUtil.isEmpty(valueMap)) {
					continue;
				}

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

				for (MessageResource resource: resources) {
					localizedString = valueMap.get(resource) == null ? localizedString : valueMap.get(resource);
					if (!CoreConstants.EMPTY.equals(localizedString)) {
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

	@Override
	public void onApplicationEvent(ApplicationEvent event) {
		if (event instanceof RepositoryStartedEvent) {
			getLocalizations();
		} else if (event instanceof LocalizationChangedEvent) {
			this.localizations = null;
			getLocalizations();
		}
	}

}