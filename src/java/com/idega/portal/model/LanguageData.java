package com.idega.portal.model;

import java.io.Serializable;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class LanguageData implements Serializable {

	private static final long serialVersionUID = 2867046722173646501L;

	private String locale;

	private String language;
	
	private Map<String, String> localizedStrings;

	public String getLocale() {
		return locale;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}
	
	public Map<String, String> getLocalizedStrings() {
		return localizedStrings;
	}

	public void setLocalizedStrings(Map<String, String> localizedStrings) {
		this.localizedStrings = localizedStrings;
	}
}