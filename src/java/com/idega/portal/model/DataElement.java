package com.idega.portal.model;

import org.codehaus.jackson.annotate.JsonCreator;
import org.codehaus.jackson.annotate.JsonValue;

import com.idega.util.StringUtil;

public enum DataElement {

	ALL,
	GENERAL,
	ADDRESS,
	CONTACT,
	PHONE,
	EMAIL,
	IMAGE,
	GENDER,
	FAMILY,
	LOGIN;

	@JsonCreator
	public static DataElement fromValue(String value) {
		if (StringUtil.isEmpty(value)) {
			return null;
		}

		for (DataElement element: values()) {
			if (element.toString().equals(value)) {
				return element;
			}
		}
		return null;
	}

	@JsonValue
	public String toValue() {
		for (DataElement element: values()) {
			if (element == this) {
				return element.toString();
			}
		}
		return null;
	}

}