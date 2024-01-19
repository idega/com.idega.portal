package com.idega.portal.business.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;

import javax.ejb.FinderException;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginInfo;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.dao.PhoneDAO;
import com.idega.core.file.data.ICFile;
import com.idega.core.location.business.AddressBusiness;
import com.idega.core.location.data.Country;
import com.idega.core.location.data.CountryHome;
import com.idega.data.IDOLookup;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.business.UserProfileHelper;
import com.idega.portal.model.DataElement;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.EmailValidator;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.sun.jersey.api.client.ClientResponse.Status;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class UserProfileHelperImpl extends DefaultSpringBean implements UserProfileHelper {

	@Override
	public Result setProfile(IWContext iwc, UserProfile profile, User user) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));
		return setProfile(iwc, iwrb, profile, user, true);
	}

	@Override
	public Result setProfile(IWContext iwc, IWResourceBundle iwrb, UserProfile profile, User user, boolean clearCaches) {
		Result result = new Result(Boolean.FALSE.toString(), iwrb == null ? null : iwrb.getLocalizedString("citizen_profile.please_provide_data", "Please provide data"));

		if (profile == null) {
			getLogger().warning("Data not provided");
			return result;
		}

		String errors = CoreConstants.EMPTY;
		try {
			if (user == null) {
				getLogger().warning("User is unknown");
				return result;
			}

			Locale locale = iwc.getCurrentLocale();
			UserBusiness userBusiness = getServiceInstance(iwc, UserBusiness.class);

			List<DataElement> dataToLoad = null;
			if (profile.getFilter() == null || ListUtil.isEmpty(profile.getFilter().getDataToLoad())) {
				dataToLoad = Arrays.asList(DataElement.values());
			} else {
				dataToLoad = profile.getFilter().getDataToLoad();
			}

			//Get the IDO user
			com.idega.user.data.User userIDO = null;
			String userPersonalId = CoreConstants.EMPTY;
			if (profile != null && (!StringUtil.isEmpty(profile.getPersonalID()) || !StringUtil.isEmpty(profile.getPersonalId()))) {
				userPersonalId = profile.getPersonalID();
				userPersonalId = StringUtil.isEmpty(userPersonalId) ? profile.getPersonalId() : userPersonalId;
				userIDO = getUserByPersonalId(userPersonalId);
			}
			if (userIDO == null) {
				userIDO = getLegacyUser(user);
				getLogger().info("Editing profile (" + profile + ") of " + user);
			}

			//Change the user data
			if (userIDO != null) {
				//**** Update the main user data ****
				//Name and personal ID
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.GENERAL)) {
					if (!StringUtil.isEmpty(profile.getName())) {
						userIDO.setFullName(profile.getName());
					}

					if (!StringUtil.isEmpty(userPersonalId)) {
						userIDO.setPersonalID(userPersonalId);
					}
					userIDO.store();

					try {
						//Allow 2-STEP authorization
						if (profile.getAllow2StepAuth() != null) {
							userIDO.setMetaData(PortalConstants.METADATA_USER_2_STEP_AUTH_SELECTED, profile.getAllow2StepAuth().toString());
						} else {
							userIDO.setMetaData(PortalConstants.METADATA_USER_2_STEP_AUTH_SELECTED, "false");
						}
						userIDO.store();
					} catch (Exception e) {}

				}

				//**** Change member username and password *****
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.LOGIN)) {
					try {
						LoginTable loginTable = LoginDBHandler.getUserLogin(userIDO);
						String login = StringUtil.isEmpty(profile.getUsername()) ?
								userIDO.getPersonalID() :
								StringUtil.isEmpty(profile.getUsername()) ?
										profile.getLogin() :
										profile.getUsername();
						login = StringUtil.isEmpty(login) ? profile.getLogin() : login;
						if (loginTable == null) {
							String password = profile.getPassword();
							if (!StringUtil.isEmpty(login) && !StringUtil.isEmpty(password)) {
								loginTable = LoginDBHandler.createLogin(
										userIDO,
										login,
										password,
										Boolean.TRUE,
										IWTimestamp.RightNow(),
										-1,
										Boolean.FALSE,
										Boolean.TRUE,
										Boolean.FALSE,
										null
								);
							}
						} else {
							boolean changeUsername = !StringUtil.isEmpty(login) && !login.equals(loginTable.getUserLogin());
							boolean changePassword = !StringUtil.isEmpty(profile.getNewPassword()) && !StringUtil.isEmpty(profile.getNewPasswordRepeat());

							if (changeUsername || changePassword) {
								String userLogin = loginTable.getUserLogin();
								String userPassword = null;

								if (changeUsername) {
									userLogin = login;
								}

								if (changePassword) {
									String passwordVerificationErrors = profile.getClass().getName().equals(UserProfile.class.getName()) ? verifyPassword(loginTable, profile) : null;
									if (StringUtil.isEmpty(passwordVerificationErrors)) {
										userPassword = profile.getNewPassword();
									} else {
										getLogger().warning("Password verification error: " + passwordVerificationErrors + ". Profile class: " + profile.getClass().getName());
										errors += passwordVerificationErrors;
									}
								}

								if (userPassword == null) {
									loginTable.setUserLogin(userLogin);
									loginTable.setLastChanged(IWTimestamp.getTimestampRightNow());
									loginTable.store();
									CoreUtil.clearAllCaches();
								} else {
									LoginDBHandler.updateLogin(((Integer) userIDO.getPrimaryKey()).intValue(), userLogin, userPassword);
								}

								LoginInfo info = LoginDBHandler.getLoginInfo(loginTable);
								info.store();
							}
						}
					} catch (Exception e) {
						getLogger().log(Level.WARNING, "Could not change the user password for member with personal id: " + userPersonalId, e);
						String errMsg = iwrb == null ?
								CoreConstants.EMPTY :
								iwrb.getLocalizedString("user_update.could_not_change_password.error", "Could not change the user password for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//Emails
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.EMAIL)) {
					try {
						//Validate emails
						Set<String> validEmails = new HashSet<>();
						if (!StringUtil.isEmpty(profile.getEmail())) {
							String[] emailsArray = profile.getEmail().replace(CoreConstants.SPACE, CoreConstants.EMPTY).split(CoreConstants.COMMA);
							if (emailsArray != null) {
								List<String> emailsList = Arrays.asList(emailsArray);
								if (!ListUtil.isEmpty(emailsList)) {
									EmailValidator emailValidator = EmailValidator.getInstance();
									for (String emailIn : emailsList) {
										if (emailValidator.validateEmail(emailIn)) {
											validEmails.add(emailIn);
										} else {
											getLogger().log(Level.WARNING, "Invalid email: " + emailIn + " for member with personal id: " + userPersonalId);
											String errMsg = iwrb == null ?
													CoreConstants.EMPTY :
													iwrb.getLocalizedString("user_update.invalid_email.error", "Invalid email: {0} was not saved. \n");
											errors += MessageFormat.format(errMsg, emailIn);
										}
									}
								}
							}
						}

						//Save emails
						if (!ListUtil.isEmpty(validEmails)) {
							userBusiness.updateUserMail(userIDO, validEmails.iterator().next());
						}
					} catch (Exception eEmails) {
						getLogger().log(Level.WARNING, "Could not change the user emails for user with personal id: " + userPersonalId, eEmails);
						String errMsg = iwrb == null ?
								CoreConstants.EMPTY :
								iwrb.getLocalizedString("user_update.could_not_change_emails.error", "Could not change the user emails for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//Phones
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.PHONE)) {
					try {
						//Get phones from string and validate them
						List<String> validPhonesList = new ArrayList<>();
						if (!StringUtil.isEmpty(profile.getPhone())) {
							String[] phonesArray = profile.getPhone().replace(CoreConstants.SPACE, CoreConstants.EMPTY).split(CoreConstants.COMMA);
							if (phonesArray != null) {
								List<String> phonesList = Arrays.asList(phonesArray);
								if (!ListUtil.isEmpty(phonesList)) {
									for (String phonein : phonesList) {
										if (StringHandler.isNumeric(phonein)) {
											validPhonesList.add(phonein);
										} else {
											getLogger().log(Level.WARNING, "Invalid phone: " + phonein + " for member with personal id: " + userPersonalId);
											String errMsg = iwrb == null ?
													CoreConstants.EMPTY :
													iwrb.getLocalizedString("user_update.invalid_phone.error", "Invalid phone: {0} was not saved. \n");
											errors += MessageFormat.format(errMsg, phonein);
										}
									}
								}
							}
						}

						//Save phones
						PhoneDAO phoneDAO = ELUtil.getInstance().getBean(PhoneDAO.class);
						//Remove all phones
						phoneDAO.removeAllByUserId(Integer.valueOf(userIDO.getPrimaryKey().toString()));
						//Create phones
						if (!ListUtil.isEmpty(validPhonesList)) {
							phoneDAO.update(Integer.valueOf(userIDO.getPrimaryKey().toString()), validPhonesList);
						}
					} catch (Exception ePhones) {
						getLogger().log(Level.WARNING, "Could not change the user phones for user with personal id: " + userPersonalId, ePhones);
						String errMsg = iwrb == null ?
								CoreConstants.EMPTY :
								iwrb.getLocalizedString("user_update.could_not_change_phones.error", "Could not change the user phones for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//Address
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.ADDRESS)) {
					try {
						AddressBusiness addressBusiness = getServiceInstance(AddressBusiness.class);
						com.idega.core.location.data.Address address = userIDO.getUsersMainAddress();

						if (profile.getAddress() != null) {
							Country country = addressBusiness.getCountryHome().findByCountryName(profile.getAddress().getCountry());
							com.idega.core.location.data.PostalCode pc = null;
							if (profile.getAddress().getPostalCodeId() != null) {
								pc = addressBusiness.getPostalCodeHome().findByPrimaryKey(profile.getAddress().getPostalCodeId());
							} else {
								pc = addressBusiness.getPostalCodeAndCreateIfDoesNotExist(profile.getAddress().getPostalCode(), profile.getAddress().getCity(), country);
							}
							String streetAddress = profile.getAddress().getStreetAddress();
							String streetName = addressBusiness.getStreetNameFromAddressString(streetAddress);
							String streetNumber = addressBusiness.getStreetNumberFromAddressString(streetAddress);
							if (address != null) {
								address.setStreetAddressNominative(streetAddress);
								address.setStreetName(streetName);
								address.setStreetNumber(streetNumber);
								address.setPostalCode(pc);
								address.setCity(profile.getAddress().getCity());
								address.store();
							} else {
								userBusiness.updateUsersMainAddressOrCreateIfDoesNotExist(
										userIDO,
										streetAddress,
										pc,
										country,
										profile.getAddress().getCity(),
										null,
										null,
										null
								);
							}
						} else if (
								!StringUtil.isEmpty(profile.getStreetAndNumber()) ||
								!StringUtil.isEmpty(profile.getPostalCode()) ||
								!StringUtil.isEmpty(profile.getCity())
						) {
							String streetAndNumber = profile.getStreetAndNumber();
							String postalCode = profile.getPostalCode();
							String city = profile.getCity();

							if (address == null) {
								com.idega.core.location.data.PostalCode pc = addressBusiness.getPostalCodeAndCreateIfDoesNotExist(postalCode, city);

								userBusiness.updateUsersMainAddressOrCreateIfDoesNotExist(
										userIDO,
										streetAndNumber,
										pc,
										null,
										city,
										null,
										null,
										null
								);
							} else {
								if (StringUtil.isEmpty(streetAndNumber)) {
									streetAndNumber = address.getStreetAddress();
								}

								if (StringUtil.isEmpty(postalCode)) {
									postalCode = address.getPostalCode() == null ? null : address.getPostalCode().getPostalCode();
								}

								if (StringUtil.isEmpty(city)) {
									city = address.getCity();
								}

								Country country = address.getCountry();
								if (country == null) {
									String countryISO = locale.getCountry();
									if (!StringUtil.isEmpty(countryISO)) {
										try {
											CountryHome countryHome = (CountryHome) IDOLookup.getHome(Country.class);
											country = countryHome.findByIsoAbbreviation(countryISO);
										} catch (Exception e) {}
									}
								}
								com.idega.core.location.data.PostalCode pc = null;
								if (!StringUtil.isEmpty(postalCode)) {
									pc = country == null ?
										addressBusiness.getPostalCodeAndCreateIfDoesNotExist(postalCode, city) :
										addressBusiness.getPostalCodeAndCreateIfDoesNotExist(postalCode, city, country);
								}

								String streetName = addressBusiness.getStreetNameFromAddressString(streetAndNumber);
								String streetNumber = addressBusiness.getStreetNumberFromAddressString(streetAndNumber);

								address.setStreetName(streetName);
								address.setStreetNumber(streetNumber);
								address.setStreetAddressNominative(streetAndNumber);
								address.setPostalCode(pc);
								address.setCity(city);
								address.store();
							}

						}
					} catch (Exception eAddr) {
						getLogger().log(Level.WARNING, "Could not update the user address for user with personal id: " + userPersonalId, eAddr);
						String errMsg = iwrb == null ?
								CoreConstants.EMPTY :
								iwrb.getLocalizedString(
								"user_update.could_not_update_user_address.error",
								"Could not update the user address for user with personal id: {0}. \n"
						);
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//User gender
				if (dataToLoad.contains(DataElement.GENDER) || dataToLoad.contains(DataElement.ALL)) {
					Integer genderIdFromProfile = profile.getGenderId();
					if (genderIdFromProfile != null) {
						try {
							userIDO.setGender(genderIdFromProfile);
							userIDO.store();
						} catch (Exception e) {}
					}
				}

				//**** User photo/picture ****
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.IMAGE)) {
					//Remove image
					if (profile.getRemovePicture() != null && profile.getRemovePicture()) {
						userIDO.setSystemImageID(null);
						userIDO.store();
					}
					//Add image
					if (!StringUtil.isEmpty(profile.getPicture()) && !StringUtil.isEmpty(profile.getPictureName())) {
						Integer fileId = saveImage(profile.getPictureName(), profile.getPicture(), profile.getPictureMimeType());
						if (fileId != null) {
							userIDO.setSystemImageID(fileId);
							userIDO.store();
						}
					}
				}

				//Save contact flags
				if (dataToLoad.contains(DataElement.CONTACT_BY) || dataToLoad.contains(DataElement.ALL)) {
					try {
						if (profile.getContactByEmail() != null) {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_EMAIL, profile.getContactByEmail().toString());
						} else {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_EMAIL, "false");
						}
						if (profile.getContactBySMS() != null) {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_SMS, profile.getContactBySMS().toString());
						} else {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_SMS, "false");
						}
						if (profile.getContactByMyMessages() != null) {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_MY_MESSAGES, profile.getContactByMyMessages().toString());
						} else {
							userIDO.setMetaData(PortalConstants.METADATA_CONTACT_BY_MY_MESSAGES, "false");
						}
						userIDO.store();
					} catch (Exception e) {}
				}
			}
		} catch (Exception e) {
			String errorMsg = "Error updating user: " + e.getLocalizedMessage();
			getLogger().log(Level.WARNING, errorMsg, e);
			errors += errorMsg;
			errors += " \n";
		} finally {
			if (StringUtil.isEmpty(errors) && clearCaches) {
				CoreUtil.clearIDOCaches();
			}
		}

		//Create return result
		if (StringUtil.isEmpty(errors)) {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.TRUE.toString());
			result.setValue(null);
		} else {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.FALSE.toString());
			result.setValue(errors);
			getLogger().warning("Errors: " + errors);
		}

		return result;
	}

	@Override
	public com.idega.user.data.User getUserByPersonalId(String personalId) {
		try {
			UserBusiness userBusiness = getServiceInstance(UserBusiness.class);
			com.idega.user.data.User user = userBusiness.getUser(personalId);
			return user;
		} catch (FinderException fe) {
			getLogger().log(Level.WARNING, "User with personal ID '" + personalId + "' does not exist", fe);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting user with personal ID: " + personalId, e);
		}

		return null;
	}

	/**
	 * Verify user's new passwords
	 * @param loginTable Login table
	 * @param user User data from web service
	 * @return Error messages if any exist
	 */
	private String verifyPassword(LoginTable loginTable, UserProfile profile) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));

		StringBuffer errors = new StringBuffer();
		if (loginTable != null) {
			if (StringUtil.isEmpty(profile.getNewPassword())) {
				errors.append(iwrb.getLocalizedString("password_verification.new_password_empty.error", "New user's password is empty. \n"));
			}
			if (StringUtil.isEmpty(profile.getNewPasswordRepeat())) {
				errors.append(iwrb.getLocalizedString("password_verification.new_repeat_password_empty.error", "New user's repeat password is empty. \n"));
			}
			if (!StringUtil.isEmpty(profile.getNewPassword()) && !StringUtil.isEmpty(profile.getNewPasswordRepeat())) {
				if (!profile.getNewPassword().equals(profile.getNewPasswordRepeat())) {
					errors.append(iwrb.getLocalizedString(
							"password_verification.new_and_repeat_passwords_different.error",
							"New user's password and repeat password are not the same. \n")
					);
				}
			}
			if (!StringUtil.isEmpty(profile.getNewPassword())) {
				if (profile.getNewPassword().length() < PortalConstants.MIN_PASSWORD_LENGTH) {
					String errMsg = iwrb.getLocalizedString(
							"password_verification.new_password_too_short.error",
							"New user's password is too short. It should be at least {0} symbols length. \n"
					);
					errors.append(MessageFormat.format(errMsg, String.valueOf(PortalConstants.MIN_PASSWORD_LENGTH)));
				}
			}
		} else {
			errors.append(iwrb.getLocalizedString("password_verification.could_not_find_login.error", "Could not find the login for the given user. \n"));
		}

		return errors.toString();
	}

	private Integer saveImage(String imageFileName, String encodedImage, String mimeType) {
		if (!StringUtil.isEmpty(encodedImage) && !StringUtil.isEmpty(imageFileName)) {
			try {
				byte [] imgBytes = Base64.getDecoder().decode(encodedImage);
				InputStream imageData = new ByteArrayInputStream(imgBytes);
				ICFile file = ((com.idega.core.file.data.ICFileHome) com.idega.data.IDOLookup.getHome(ICFile.class)).create();
				file.setName(imageFileName);
				file.setCreationDate(IWTimestamp.getTimestampRightNow());
				file.setMimeType(mimeType);
				file.setFileValue(imageData);
				file.setPublic(true);
				file.store();
				Integer fileId = ((Integer) file.getPrimaryKey()).intValue();
				return fileId;
			} catch (Exception e) {
				getLogger().log(Level.WARNING, "Could not save the picture/image: " + e.getLocalizedMessage(), e);
			}
		}
		return null;
	}

}