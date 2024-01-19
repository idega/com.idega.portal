package com.idega.portal.model;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ejb.FinderException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.oauth2.server.authentication.bean.User;
import com.idega.business.IBOLookup;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.contact.dao.PhoneDAO;
import com.idega.core.contact.data.bean.Email;
import com.idega.core.contact.data.bean.Phone;
import com.idega.core.location.data.bean.Address;
import com.idega.core.location.data.bean.Country;
import com.idega.core.location.data.bean.PostalCode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.presentation.Image;
import com.idega.user.bean.UserDataBean;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.Gender;
import com.idega.user.helpers.UserHelper;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.EmailValidator;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class UserProfile extends User {

	private static final long serialVersionUID = -7263530675290796371L;

	private String personalId;
	private String phone;

	private String dateOfBirth;

	private String picture;
	private String pictureMimeType;
	private String pictureName;
	private String pictureURI;
	private String pictureExtension;
	private Boolean removePicture;

	private List<UserProfile> familyMembers;
	private String relation;

	private Integer genderId;
	private String gender;

	private String fullAddress;
	private String streetAndNumber;
	private String postalCode;
	private String city;

	private String username;
	private String password;
	private String newPassword;
	private String newPasswordRepeat;

	private Integer id;

	private String uuid;

	private Filter filter;

	private Boolean selected;

	private Boolean contactByEmail;
	private Boolean contactBySMS;
	private Boolean contactByMyMessages;

	private Boolean allow2StepAuth = Boolean.FALSE;
	private Boolean portal2StepAuthSelectable = Boolean.FALSE;


	public UserProfile() {
		super();
	}

	public UserProfile(com.idega.user.data.bean.User user) {
		this();

		if (user == null) {
			return;
		}

		id = user.getId();
		personalId = user.getPersonalID();
		setEmail(user.getEmailAddress());
		List<Phone> phones = user.getPhones();
		if (!ListUtil.isEmpty(phones)) {
			phone = phones.iterator().next().getNumber();
		}
		setName(user.getName());
		uuid = user.getUniqueId();
		LoginTable loginTable = LoginDBHandler.getUserLogin(user.getId());
		if (loginTable != null) {
			this.username = loginTable.getUserLogin();
		}

		//Get the contact flags
		String contactByEmailStr = user.getMetaData(PortalConstants.METADATA_CONTACT_BY_EMAIL);
		if (!StringUtil.isEmpty(contactByEmailStr)) {
			setContactByEmail(Boolean.valueOf(contactByEmailStr));
		}
		String contactBySMSStr = user.getMetaData(PortalConstants.METADATA_CONTACT_BY_SMS);
		if (!StringUtil.isEmpty(contactBySMSStr)) {
			setContactBySMS(Boolean.valueOf(contactBySMSStr));
		}
		String contactByMyMessagesStr = user.getMetaData(PortalConstants.METADATA_CONTACT_BY_MY_MESSAGES);
		if (!StringUtil.isEmpty(contactByMyMessagesStr)) {
			setContactByMyMessages(Boolean.valueOf(contactByMyMessagesStr));
		}

		//Allow 2-STEP auth
		String allow2StepAuthTmp = user.getMetaData(PortalConstants.METADATA_USER_2_STEP_AUTH_SELECTED);
		if (!StringUtil.isEmpty(allow2StepAuthTmp)) {
			setAllow2StepAuth(Boolean.valueOf(allow2StepAuthTmp));
		} else {
			setAllow2StepAuth(Boolean.FALSE);
		}
		//Portal 2-STEP auth is selectable
		IWMainApplication application = IWMainApplication.getDefaultIWMainApplication();
		setPortal2StepAuthSelectable(
				application.getSettings().getBoolean(PortalConstants.APP_PROPERTY_USE_2_STEP_AUTH, false) && application.getSettings().getBoolean(PortalConstants.APP_PROPERTY_2_STEP_AUTH_SELECTABLE, false)
		);

	}

	public UserProfile(com.idega.user.data.User user) {
		this();

		if (user == null) {
			return;
		}

		id = Integer.valueOf(user.getId());
		personalId = user.getPersonalID();
		setPersonalID(user.getPersonalID());
		com.idega.core.contact.data.Email email = null;
		try {
			email = user.getUsersEmail();
		} catch (Exception e) {}
		if (email != null) {
			setEmail(email.getEmailAddress());
		}

		PhoneDAO phoneDAO = ELUtil.getInstance().getBean(PhoneDAO.BEAN_NAME);
		Collection<Phone> phones = phoneDAO.getPhones(id);
		if (!ListUtil.isEmpty(phones)) {
			for (Iterator<Phone> iter = phones.iterator(); (iter.hasNext() && StringUtil.isEmpty(phone));) {
				Phone ph = iter.next();
				String number = ph == null ? null : ph.getNumber();
				if (StringUtil.isEmpty(number)) {
					continue;
				}

				phone = number;
			}
		}
		setName(user.getName());
		uuid = user.getUniqueId();
		LoginTable loginTable = LoginDBHandler.getUserLogin(id);
		if (loginTable != null) {
			this.username = loginTable.getUserLogin();
		}

		com.idega.core.location.data.Address address = null;
		try {
			UserBusiness userBusiness = IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), UserBusiness.class);
			address = userBusiness.getUsersMainAddress(user);
		} catch (Exception e) {}
		if (address != null) {
			UserHelper userHelper = ELUtil.getInstance().getBean(UserHelper.USER_HELPER_BEAN);
			UserDataBean userDataBean = new UserDataBean();
			userHelper.setAddress(userDataBean, address);

			StringBuilder sb = new StringBuilder();

			streetAndNumber = userDataBean.getStreetNameAndNumber();
			String line = streetAndNumber;
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

			city = userDataBean.getCity();
			line = city;
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

			postalCode = userDataBean.getPostalCodeId();
			line = userDataBean.getPostalBox();
			if (!StringUtil.isEmpty(line)) {
				if (sb.length() > 0) {
					sb.append(CoreConstants.COMMA)
					.append(CoreConstants.SPACE);
				}

				sb.append(line);
			}

			if (!StringUtil.isEmpty(sb.toString())) {
				this.fullAddress = sb.toString();
			}
		}

		//Allow 2-STEP auth
		String allow2StepAuthTmp = user.getMetaData(PortalConstants.METADATA_USER_2_STEP_AUTH_SELECTED);
		if (!StringUtil.isEmpty(allow2StepAuthTmp)) {
			setAllow2StepAuth(Boolean.valueOf(allow2StepAuthTmp));
		} else {
			setAllow2StepAuth(Boolean.FALSE);
		}
		//Portal 2-STEP auth is selectable
		IWMainApplication application = IWMainApplication.getDefaultIWMainApplication();
		setPortal2StepAuthSelectable(
				application.getSettings().getBoolean(PortalConstants.APP_PROPERTY_USE_2_STEP_AUTH, false) && application.getSettings().getBoolean(PortalConstants.APP_PROPERTY_2_STEP_AUTH_SELECTABLE, false)
		);

	}

	public UserProfile(com.idega.user.data.bean.User user, Locale locale, DataElement... interestedIn) {
		super(user);

		if (user == null) {
			return;
		}

		com.idega.user.data.User userIDO = null;
		try {
			userIDO = getUserByPersonalId(user.getPersonalID());
		} catch (Exception e) {
			Logger.getLogger(getClass().getName()).log(Level.WARNING, "Could not get the user with personal id: " + user.getPersonalID(), e);
		}

		List<DataElement> dataToLoad = ArrayUtil.isEmpty(interestedIn) ? Arrays.asList(DataElement.values()) : Arrays.asList(interestedIn);

		IWResourceBundle iwrb = IWMainApplication.getDefaultIWMainApplication().getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(CoreUtil.getCurrentLocale());

		//	Address
		if (dataToLoad.contains(DataElement.ADDRESS) || dataToLoad.contains(DataElement.ALL)) {
			Address address = user.getMainAddress();
			if (address == null) {
				List<Address> addresses = user.getAddresses();
				if (!ListUtil.isEmpty(addresses)) {
					for (Iterator<Address> iter = addresses.iterator(); (iter.hasNext() && address == null);) {
						address = iter.next();
					}
				}
			}
			if (address != null) {
				setAddress(new com.idega.block.oauth2.server.authentication.bean.Address(address, locale));
				this.streetAndNumber = address.getAddress();
				this.city = address.getCity();
				try {
					com.idega.core.location.data.PostalCode postalCode = userIDO.getUsersMainAddress().getPostalCode();
					if (postalCode != null) {
						this.postalCode = postalCode.getPostalCode();
					}
				} catch (Exception e) {}
				if (StringUtil.isEmpty(this.postalCode)) {
					PostalCode postalCode = address.getPostalCode();
					if (postalCode != null) {
						this.postalCode = postalCode.getPostalCode();
					}
				}

				//Full address
				StringBuilder sbFullAddress = new StringBuilder();
				String line = this.streetAndNumber;
				if (!StringUtil.isEmpty(line)) {
					sbFullAddress.append(line);
				}

				line = this.postalCode;
				if (!StringUtil.isEmpty(line)) {
					if (sbFullAddress.length() > 0) {
						sbFullAddress.append(CoreConstants.COMMA)
						.append(CoreConstants.SPACE);
					}
					sbFullAddress.append(line);
				}

				line = this.city;
				if (!StringUtil.isEmpty(line)) {
					if (sbFullAddress.length() > 0) {
						sbFullAddress.append(CoreConstants.COMMA)
						.append(CoreConstants.SPACE);
					}
					sbFullAddress.append(line);
				}

				Country country = address.getCountry();
				if (country != null) {
					line = country.getName();
					if (!StringUtil.isEmpty(line)) {
						if (sbFullAddress.length() > 0) {
							sbFullAddress.append(CoreConstants.COMMA)
							.append(CoreConstants.SPACE);
						}

						sbFullAddress.append(line);
					}
				}

				if (!StringUtil.isEmpty(sbFullAddress.toString())) {
					this.fullAddress = sbFullAddress.toString();
				}

			}
		}

		//Contact data
		if (dataToLoad.contains(DataElement.CONTACT) || dataToLoad.contains(DataElement.ALL)) {
			setPhones(user.getPhones());
			setEmails(user.getEmails());
		} else {
			if (dataToLoad.contains(DataElement.PHONE)) {
				setPhones(user.getPhones());
			}
			if (dataToLoad.contains(DataElement.EMAIL)) {
				setEmails(user.getEmails());
			} else if (dataToLoad.contains(DataElement.GENERAL)) {
				setEmail(user.getEmailAddress());
			}
		}

		//User gender
		if (dataToLoad.contains(DataElement.GENDER) || dataToLoad.contains(DataElement.ALL)) {
			Gender gender = user.getGender();
			if (gender == null) {
				try {
					UserBusiness userBusiness = IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), UserBusiness.class);
					com.idega.user.data.Gender genderLegacy = userBusiness.getGenderFromPersonalId(user.getPersonalID());
					this.gender = genderLegacy == null ? null : iwrb.getLocalizedString("gender." + genderLegacy.getName(), genderLegacy.getName());
					this.genderId = Integer.valueOf(genderLegacy.getPrimaryKey().toString());
				} catch (Exception e) {}
			} else {
				this.gender = gender == null ? null : iwrb.getLocalizedString("gender." + gender.getName(), gender.getName());
				this.genderId = gender.getId();
			}
		}

		//User image
		if (dataToLoad.contains(DataElement.IMAGE) || dataToLoad.contains(DataElement.ALL)) {
			try {
				Image userImage = null;
				try {
					if (userIDO != null) {
						userImage = new Image(userIDO.getSystemImageID());
					}
				} catch (Exception e) {
					Logger.getLogger(getClass().getName()).log(Level.WARNING, "Could not get the user image for user with personal id: " + user.getPersonalID(), e);
				}

				if (userImage != null) {
					this.pictureURI = userImage.getMediaURL(IWMainApplication.getDefaultIWApplicationContext());
				}
			} catch (Exception e) {
				Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting user's image for user: " + user, e);
			}
		}

		//Login
		if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.LOGIN)) {
			try {
				LoginTable loginTable = LoginDBHandler.getUserLogin(((Integer) userIDO.getPrimaryKey()).intValue());
				if (loginTable != null) {
					setLogin(loginTable.getUserLogin());
				}
			} catch (Exception eLogin) {
				Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting user's login for user: " + user, eLogin);
			}
		}

		//Get the contact flags
		if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.CONTACT_BY) && userIDO != null) {
			try {
				String contactByEmailStr = userIDO.getMetaData(PortalConstants.METADATA_CONTACT_BY_EMAIL);
				if (!StringUtil.isEmpty(contactByEmailStr)) {
					setContactByEmail(Boolean.valueOf(contactByEmailStr));
				}
				String contactBySMSStr = userIDO.getMetaData(PortalConstants.METADATA_CONTACT_BY_SMS);
				if (!StringUtil.isEmpty(contactBySMSStr)) {
					setContactBySMS(Boolean.valueOf(contactBySMSStr));
				}
				String contactByMyMessagesStr = userIDO.getMetaData(PortalConstants.METADATA_CONTACT_BY_MY_MESSAGES);
				if (!StringUtil.isEmpty(contactByMyMessagesStr)) {
					setContactByMyMessages(Boolean.valueOf(contactByMyMessagesStr));
				}
			} catch (Exception eLogin) {
				Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting user's CONTACT BY from metadata for user: " + userIDO, eLogin);
			}
		}
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public void setPhones(List<Phone> phones) {
		if (ListUtil.isEmpty(phones)) {
			return;
		}

		Set<String> numbers = new HashSet<>();
		for (Iterator<Phone> phonesIter = phones.iterator(); phonesIter.hasNext();) {
			Phone phone = phonesIter.next();
			if (phone == null) {
				continue;
			}

			String number = phone.getNumber();
			if (StringUtil.isEmpty(number)) {
				continue;
			}

			numbers.add(number);
		}

		if (ListUtil.isEmpty(numbers)) {
			return;
		}

		StringBuilder phonesLabel = new StringBuilder();
		for (Iterator<String> numbersIter = numbers.iterator(); numbersIter.hasNext();) {
			String number = numbersIter.next();
			if (StringUtil.isEmpty(number)) {
				continue;
			}

			phonesLabel.append(number);
			if (numbersIter.hasNext()) {
				phonesLabel.append(CoreConstants.COMMA).append(CoreConstants.SPACE);
			}
		}

		setPhone(phonesLabel.toString());
	}

	public void setEmails(List<Email> emails) {
		if (ListUtil.isEmpty(emails)) {
			return;
		}

		EmailValidator validator = EmailValidator.getInstance();
		Set<String> emailAddresses = new HashSet<>();
		for (Iterator<Email> emailsIter = emails.iterator(); emailsIter.hasNext();) {
			Email email = emailsIter.next();
			if (email == null) {
				continue;
			}

			String address = email.getAddress();
			if (!validator.isValid(address)) {
				continue;
			}

			emailAddresses.add(address);
		}

		if (ListUtil.isEmpty(emailAddresses)) {
			return;
		}

		StringBuilder emailsLabel = new StringBuilder();
		for (Iterator<String> emailAddressesIter = emailAddresses.iterator(); emailAddressesIter.hasNext();) {
			String address = emailAddressesIter.next();
			if (!validator.isValid(address)) {
				continue;
			}

			emailsLabel.append(address);
			if (emailAddressesIter.hasNext()) {
				emailsLabel.append(CoreConstants.COMMA).append(CoreConstants.SPACE);
			}
		}

		setEmail(emailsLabel.toString());
	}

	public String getDateOfBirth() {
		return dateOfBirth;
	}

	public void setDateOfBirth(String dateOfBirth) {
		this.dateOfBirth = dateOfBirth;
	}

	public String getPicture() {
		return picture;
	}

	public void setPicture(String picture) {
		this.picture = picture;
	}

	public String getPictureMimeType() {
		return pictureMimeType;
	}

	public void setPictureMimeType(String pictureMimeType) {
		this.pictureMimeType = pictureMimeType;
	}

	public String getPictureName() {
		return pictureName;
	}

	public void setPictureName(String pictureName) {
		this.pictureName = pictureName;
	}

	public String getPictureURI() {
		return pictureURI;
	}

	public void setPictureURI(String pictureURI) {
		this.pictureURI = pictureURI;
	}

	public String getPictureExtension() {
		return pictureExtension;
	}

	public void setPictureExtension(String pictureExtension) {
		this.pictureExtension = pictureExtension;
	}

	public Boolean getRemovePicture() {
		return removePicture;
	}

	public void setRemovePicture(Boolean removePicture) {
		this.removePicture = removePicture;
	}

	public List<UserProfile> getFamilyMembers() {
		return familyMembers;
	}

	public void setFamilyMembers(List<UserProfile> familyMembers) {
		this.familyMembers = familyMembers;
	}

	public String getRelation() {
		return relation;
	}

	public void setRelation(String relation) {
		this.relation = relation;
	}

	public Integer getGenderId() {
		return genderId;
	}

	public void setGenderId(Integer genderId) {
		this.genderId = genderId;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public String getStreetAndNumber() {
		return streetAndNumber;
	}

	public void setStreetAndNumber(String streetAndNumber) {
		this.streetAndNumber = streetAndNumber;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public Filter getFilter() {
		return filter;
	}

	public void setFilter(Filter filter) {
		this.filter = filter;
	}

	private com.idega.user.data.User getUserByPersonalId(String personalId) {
		try {
			UserBusiness userBusiness = getUserBusiness();
			com.idega.user.data.User user = userBusiness.getUser(personalId);
			return user;
		} catch (FinderException fe) {
			Logger.getLogger(getClass().getName()).warning("User with personal ID '" + personalId + "' does not exist");
		} catch (Exception e) {
			Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting user with personal ID: " + personalId, e);
		}

		return null;
	}

	private UserBusiness getUserBusiness() {
		try {
			return IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), UserBusiness.class);
		} catch (Exception e) {}
		return null;
	}

	public String getNewPassword() {
		return newPassword;
	}

	public void setNewPassword(String newPassword) {
		this.newPassword = newPassword;
	}

	public String getNewPasswordRepeat() {
		return newPasswordRepeat;
	}

	public void setNewPasswordRepeat(String newPasswordRepeat) {
		this.newPasswordRepeat = newPasswordRepeat;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getPersonalId() {
		if (StringUtil.isEmpty(personalId)) {
			return getPersonalID();
		}
		return personalId;
	}

	public void setPersonalId(String personalId) {
		this.personalId = personalId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	@Override
	public String getUuid() {
		return uuid;
	}

	@Override
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getFullAddress() {
		return fullAddress;
	}

	public void setFullAddress(String fullAddress) {
		this.fullAddress = fullAddress;
	}

	public Boolean getSelected() {
		return selected;
	}

	public void setSelected(Boolean selected) {
		this.selected = selected;
	}

	public Boolean getContactByEmail() {
		return contactByEmail;
	}

	public void setContactByEmail(Boolean contactByEmail) {
		this.contactByEmail = contactByEmail;
	}

	public Boolean getContactBySMS() {
		return contactBySMS;
	}

	public void setContactBySMS(Boolean contactBySMS) {
		this.contactBySMS = contactBySMS;
	}

	public Boolean getContactByMyMessages() {
		return contactByMyMessages;
	}

	public void setContactByMyMessages(Boolean contactByMyMessages) {
		this.contactByMyMessages = contactByMyMessages;
	}

	public Boolean getAllow2StepAuth() {
		return allow2StepAuth;
	}

	public void setAllow2StepAuth(Boolean allow2StepAuth) {
		this.allow2StepAuth = allow2StepAuth;
	}

	public Boolean getPortal2StepAuthSelectable() {
		return portal2StepAuthSelectable;
	}

	public void setPortal2StepAuthSelectable(Boolean portal2StepAuthSelectable) {
		this.portal2StepAuthSelectable = portal2StepAuthSelectable;
	}

	@Override
	public String toString() {
		return "User ID: " + getId();
	}

}