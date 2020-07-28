package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class OAuth implements Serializable {

	private static final long serialVersionUID = 7780279080745199758L;

	private OAuthInfo defaultClient;

	private OAuthInfo unexpiringClient;

	private List<OAuthInfo> allClients;

	public OAuthInfo getDefaultClient() {
		return defaultClient;
	}

	public void setDefaultClient(OAuthInfo defaultClient) {
		this.defaultClient = defaultClient;
	}

	public OAuthInfo getUnexpiringClient() {
		return unexpiringClient;
	}

	public void setUnexpiringClient(OAuthInfo unexpiringClient) {
		this.unexpiringClient = unexpiringClient;
	}

	public List<OAuthInfo> getAllClients() {
		return allClients;
	}

	public void setAllClients(List<OAuthInfo> allClients) {
		this.allClients = allClients;
	}

}