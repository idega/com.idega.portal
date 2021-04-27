package com.idega.portal.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class UserAccount implements Serializable {

	private static final long serialVersionUID = 1929758087625187598L;

	private String username;

	private String personalId;

	private String email;

	private String password;

	private String userId;

	private String name;

	private String errorMessage;

	private String uuid;

	private boolean personAsCompany;

	private String nameOfResponsiblePerson;

	private String personalIdOfResponsiblePerson;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPersonalId() {
		return personalId;
	}

	public void setPersonalId(String personalId) {
		this.personalId = personalId;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public boolean isPersonAsCompany() {
		return personAsCompany;
	}

	public void setPersonAsCompany(boolean personAsCompany) {
		this.personAsCompany = personAsCompany;
	}

	public String getNameOfResponsiblePerson() {
		return nameOfResponsiblePerson;
	}

	public void setNameOfResponsiblePerson(String nameOfResponsiblePerson) {
		this.nameOfResponsiblePerson = nameOfResponsiblePerson;
	}

	public String getPersonalIdOfResponsiblePerson() {
		return personalIdOfResponsiblePerson;
	}

	public void setPersonalIdOfResponsiblePerson(String personalIdOfResponsiblePerson) {
		this.personalIdOfResponsiblePerson = personalIdOfResponsiblePerson;
	}

}