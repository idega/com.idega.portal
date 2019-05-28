package com.idega.portal.service;

import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.Result;

public interface LocalizationService {

	public String setLanguage(String language, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setLocalization(Localization localization, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setLocalizations(Localizations localizations, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public List<LanguageData> getAvailableLanguages(HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result addLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result removeLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Map<String, LanguageData> getLocalizations(HttpServletRequest request, HttpServletResponse response, ServletContext context);

}