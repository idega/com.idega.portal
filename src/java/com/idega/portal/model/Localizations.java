package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Localizations implements Serializable {

	private static final long serialVersionUID = 5526422386402905283L;

	private List<Localization> localizations;

	public Localizations() {
	}

	public Localizations(List<Localization> localizations) {
		this.localizations = localizations;
	}

	public List<Localization> getLocalizations() {
		return localizations;
	}

	public void setLocalizations(List<Localization> localizations) {
		this.localizations = localizations;
	}
}
