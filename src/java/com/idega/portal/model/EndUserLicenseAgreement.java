package com.idega.portal.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class EndUserLicenseAgreement implements Serializable {

	private static final long serialVersionUID = -5607025572506567380L;

	private Localization title;

	private Localization message;

	public EndUserLicenseAgreement() {
		super();
	}

	public EndUserLicenseAgreement(Localization title, Localization message) {
		this();

		this.title = title;
		this.message = message;
	}

	public Localization getTitle() {
		return title;
	}

	public void setTitle(Localization title) {
		this.title = title;
	}

	public Localization getMessage() {
		return message;
	}

	public void setMessage(Localization message) {
		this.message = message;
	}

}