package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.contact.data.bean.Phone;
import com.idega.user.bean.UserDataBean;
import com.idega.user.business.UserApplicationEngine;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

/**
 * Data structure to represent user entity
 *
 * @author valdas
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class User implements Serializable {

	private static final long serialVersionUID = 1514809950328524234L;

	private String id, personalId, email, phone, name, address, username, password, uuid;

	private Boolean selected;

	public User() {
		super();
	}

	public User(String id, String email, String phone, String name) {
		this.id = id;
		this.email = email;
		this.phone = phone;
		this.name = name;
	}

	public User(String id, String email, String phone, String name, String address) {
		this.id = id;
		this.email = email;
		this.phone = phone;
		this.name = name;
		this.address = address;
	}

	public User(com.idega.user.data.bean.User user) {
		if (user == null) {
			return;
		}

		id = String.valueOf(user.getId());
		personalId = user.getPersonalID();
		email = user.getEmailAddress();
		List<Phone> phones = user.getPhones();
		if (!ListUtil.isEmpty(phones)) {
			phone = phones.iterator().next().getNumber();
		}
		name = user.getName();
		uuid = user.getUniqueId();
		LoginTable loginTable = LoginDBHandler.getUserLogin(user.getId());
		if (loginTable != null) {
			this.username = loginTable.getUserLogin();
		}
	}

	public User(com.idega.user.data.User user) {
		if (user == null) {
			return;
		}

		id = user.getId();
		UserApplicationEngine userApplicationEngine = ELUtil.getInstance().getBean(UserApplicationEngine.class);
		UserDataBean userDataBean = userApplicationEngine.getUserInfo(user);
		email = userDataBean.getEmail();
		phone = userDataBean.getPhone();
		name = userDataBean.getName();
		uuid = user.getUniqueId();
		LoginTable loginTable = LoginDBHandler.getUserLogin(Integer.valueOf(id));
		if (loginTable != null) {
			this.username = loginTable.getUserLogin();
		}

		StringBuilder sb = new StringBuilder();

		String line = userDataBean.getStreetNameAndNumber();
		if (!StringUtil.isEmpty(line)) {
			sb.append(line);
		}

		line = userDataBean.getProvince();
		if (!StringUtil.isEmpty(line)) {
			if (sb.length() > 0) {
				sb.append(CoreConstants.COMMA)
				.append(CoreConstants.SPACE);
			}

			sb.append(line);
		}

		line = userDataBean.getCity();
		if (!StringUtil.isEmpty(line)) {
			if (sb.length() > 0) {
				sb.append(CoreConstants.COMMA)
				.append(CoreConstants.SPACE);
			}

			sb.append(line);
		}

		line = userDataBean.getCommune();
		if (!StringUtil.isEmpty(line)) {
			if (sb.length() > 0) {
				sb.append(CoreConstants.COMMA)
				.append(CoreConstants.SPACE);
			}

			sb.append(line);
		}

		line = userDataBean.getCountryName();
		if (!StringUtil.isEmpty(line)) {
			if (sb.length() > 0) {
				sb.append(CoreConstants.COMMA)
				.append(CoreConstants.SPACE);
			}

			sb.append(line);
		}

		line = userDataBean.getPostalBox();
		if (!StringUtil.isEmpty(line)) {
			if (sb.length() > 0) {
				sb.append(CoreConstants.COMMA)
				.append(CoreConstants.SPACE);
			}

			sb.append(line);
		}

		if (!StringUtil.isEmpty(sb.toString())) {
			this.address = sb.toString();
		}
	}

	/**
	 *
	 * @return {@link com.idega.user.data.User#getPersonalID()}
	 * @author <a href="mailto:martynas@idega.com">Martynas Stakė</a>
	 */
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}


	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the address
	 */
	public String getAddress() {
		return address;
	}

	/**
	 * @param address the address to set
	 */
	public void setAddress(String address) {
		this.address = address;
	}

	@Override
	public boolean equals(Object object) {
		if(object instanceof User){
			if(id == null){
				return ((User)object).getId() == null;
			}
			return id.equals(((User)object).getId());
		}
		return false;
	}

	public String getPersonalId() {
		return personalId;
	}

	public void setPersonalId(String personalId) {
		this.personalId = personalId;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public Boolean getSelected() {
		return selected;
	}

	public void setSelected(Boolean selected) {
		this.selected = selected;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

}