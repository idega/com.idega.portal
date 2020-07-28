package com.idega.portal.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class OAuthInfo implements Serializable {

	private static final long serialVersionUID = -8823830021472131236L;

	private String clientId;

	private String clientSecret;

	private String grantType;

	private Integer accessTokenValiditySeconds;

	private Integer refreshTokenValiditySeconds;

	private Boolean remember;

	public OAuthInfo() {
		super();
	}

	public OAuthInfo(String clientId, String clientSecret, String grantType, Integer accessTokenValiditySeconds, Integer refreshTokenValiditySeconds) {
		this();

		this.clientId = clientId;
		this.clientSecret = clientSecret;
		this.grantType = grantType;
		this.accessTokenValiditySeconds = accessTokenValiditySeconds;
		this.refreshTokenValiditySeconds = refreshTokenValiditySeconds;
		this.remember = accessTokenValiditySeconds != null && (accessTokenValiditySeconds == 31556926 || accessTokenValiditySeconds == 2592000);
	}

	public String getClientId() {
		return clientId;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public String getClientSecret() {
		return clientSecret;
	}

	public void setClient(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	public String getGrantType() {
		return grantType;
	}

	public void setGrantType(String grantType) {
		this.grantType = grantType;
	}

	public Integer getAccessTokenValiditySeconds() {
		return accessTokenValiditySeconds;
	}

	public void setAccessTokenValiditySeconds(Integer accessTokenValiditySeconds) {
		this.accessTokenValiditySeconds = accessTokenValiditySeconds;
	}

	public Integer getRefreshTokenValiditySeconds() {
		return refreshTokenValiditySeconds;
	}

	public void setRefreshTokenValiditySeconds(Integer refreshTokenValiditySeconds) {
		this.refreshTokenValiditySeconds = refreshTokenValiditySeconds;
	}

	public Boolean getRemember() {
		return remember;
	}

	public void setRemember(Boolean remember) {
		this.remember = remember;
	}

}