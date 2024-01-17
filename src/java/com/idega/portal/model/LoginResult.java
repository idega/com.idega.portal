package com.idega.portal.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.login.bean.OAuthToken;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.util.StringUtil;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class LoginResult implements Serializable {

	private static final long serialVersionUID = 310359328286958367L;

	private OAuthToken token;

	private List<Result> errors;

	private String redirect;

	private List<AdvancedProperty> headerParams;

	private String uniqueId;

	private String authState = AuthState.ONE_STEP.toString();


	public LoginResult() {
		super();
	}

	public LoginResult(OAuthToken token) {
		this();

		this.token = token;
	}

	public LoginResult(Integer status, String error, String errorLocalizedKey) {
		this();

		addError(status, error, errorLocalizedKey);
	}

	public LoginResult(String authState) {
		this();

		this.authState = authState;
	}

	public void addError(Integer status, String error, String errorLocalizedKey) {
		if (errors == null) {
			errors = new ArrayList<>();
		}

		if (status != null && !StringUtil.isEmpty(error)) {
			errors.add(new Result(status, errorLocalizedKey, error));
		}
	}

	public OAuthToken getToken() {
		return token;
	}

	public void setToken(OAuthToken token) {
		this.token = token;
	}

	public List<Result> getErrors() {
		return errors;
	}

	public void setErrors(List<Result> errors) {
		this.errors = errors;
	}

	public String getRedirect() {
		return redirect;
	}

	public void setRedirect(String redirect) {
		this.redirect = redirect;
	}

	public List<AdvancedProperty> getHeaderParams() {
		return headerParams;
	}

	public void setHeaderParams(List<AdvancedProperty> headerParams) {
		this.headerParams = headerParams;
	}

	public String getUniqueId() {
		return uniqueId;
	}

	public void setUniqueId(String uniqueId) {
		this.uniqueId = uniqueId;
	}

	public String getAuthState() {
		return authState;
	}

	public void setAuthState(String authState) {
		this.authState = authState;
	}




}