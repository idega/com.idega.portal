package com.idega.portal.service;

import java.util.List;
import java.util.Map;

import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.Result;

public interface LocalizationService {

	public String setLanguage(String language);

	public Result setLocalization(Localization localization);

	public Result setLocalizations(Localizations localizations);

	public List<LanguageData> getAvailableLanguages();

	public Result addLanguage(String locale);

	public Result removeLanguage(String locale);

	public Map<String, LanguageData> getLocalizations();

}