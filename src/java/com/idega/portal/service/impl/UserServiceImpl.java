/**
 * @(#)UserManagerService.java    1.0.0 14:23:55
 *
 * Idega Software hf. Source Code Licence Agreement x
 *
 * This agreement, made this 10th of February 2006 by and between
 * Idega Software hf., a business formed and operating under laws
 * of Iceland, having its principal place of business in Reykjavik,
 * Iceland, hereinafter after referred to as "Manufacturer" and Agura
 * IT hereinafter referred to as "Licensee".
 * 1.  License Grant: Upon completion of this agreement, the source
 *     code that may be made available according to the documentation for
 *     a particular software product (Software) from Manufacturer
 *     (Source Code) shall be provided to Licensee, provided that
 *     (1) funds have been received for payment of the License for Software and
 *     (2) the appropriate License has been purchased as stated in the
 *     documentation for Software. As used in this License Agreement,
 *     Licensee shall also mean the individual using or installing
 *     the source code together with any individual or entity, including
 *     but not limited to your employer, on whose behalf you are acting
 *     in using or installing the Source Code. By completing this agreement,
 *     Licensee agrees to be bound by the terms and conditions of this Source
 *     Code License Agreement. This Source Code License Agreement shall
 *     be an extension of the Software License Agreement for the associated
 *     product. No additional amendment or modification shall be made
 *     to this Agreement except in writing signed by Licensee and
 *     Manufacturer. This Agreement is effective indefinitely and once
 *     completed, cannot be terminated. Manufacturer hereby grants to
 *     Licensee a non-transferable, worldwide license during the term of
 *     this Agreement to use the Source Code for the associated product
 *     purchased. In the event the Software License Agreement to the
 *     associated product is terminated; (1) Licensee's rights to use
 *     the Source Code are revoked and (2) Licensee shall destroy all
 *     copies of the Source Code including any Source Code used in
 *     Licensee's applications.
 * 2.  License Limitations
 *     2.1 Licensee may not resell, rent, lease or distribute the
 *         Source Code alone, it shall only be distributed as a
 *         compiled component of an application.
 *     2.2 Licensee shall protect and keep secure all Source Code
 *         provided by this this Source Code License Agreement.
 *         All Source Code provided by this Agreement that is used
 *         with an application that is distributed or accessible outside
 *         Licensee's organization (including use from the Internet),
 *         must be protected to the extent that it cannot be easily
 *         extracted or decompiled.
 *     2.3 The Licensee shall not resell, rent, lease or distribute
 *         the products created from the Source Code in any way that
 *         would compete with Idega Software.
 *     2.4 Manufacturer's copyright notices may not be removed from
 *         the Source Code.
 *     2.5 All modifications on the source code by Licencee must
 *         be submitted to or provided to Manufacturer.
 * 3.  Copyright: Manufacturer's source code is copyrighted and contains
 *     proprietary information. Licensee shall not distribute or
 *     reveal the Source Code to anyone other than the software
 *     developers of Licensee's organization. Licensee may be held
 *     legally responsible for any infringement of intellectual property
 *     rights that is caused or encouraged by Licensee's failure to abide
 *     by the terms of this Agreement. Licensee may make copies of the
 *     Source Code provided the copyright and trademark notices are
 *     reproduced in their entirety on the copy. Manufacturer reserves
 *     all rights not specifically granted to Licensee.
 *
 * 4.  Warranty & Risks: Although efforts have been made to assure that the
 *     Source Code is correct, reliable, date compliant, and technically
 *     accurate, the Source Code is licensed to Licensee as is and without
 *     warranties as to performance of merchantability, fitness for a
 *     particular purpose or use, or any other warranties whether
 *     expressed or implied. Licensee's organization and all users
 *     of the source code assume all risks when using it. The manufacturers,
 *     distributors and resellers of the Source Code shall not be liable
 *     for any consequential, incidental, punitive or special damages
 *     arising out of the use of or inability to use the source code or
 *     the provision of or failure to provide support services, even if we
 *     have been advised of the possibility of such damages. In any case,
 *     the entire liability under any provision of this agreement shall be
 *     limited to the greater of the amount actually paid by Licensee for the
 *     Software or 5.00 USD. No returns will be provided for the associated
 *     License that was purchased to become eligible to receive the Source
 *     Code after Licensee receives the source code.
 */
package com.idega.portal.service.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.logging.Level;

import javax.ejb.FinderException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginInfo;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.dao.ContactDAO;
import com.idega.core.contact.dao.EMailDAO;
import com.idega.core.contact.dao.PhoneDAO;
import com.idega.core.contact.data.bean.EmailType;
import com.idega.core.file.data.ICFile;
import com.idega.core.location.business.AddressBusiness;
import com.idega.core.location.data.Country;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.DataElement;
import com.idega.portal.model.Filter;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.UserService;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.EmailValidator;
import com.idega.util.IOUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.sun.jersey.api.client.ClientResponse.Status;
import com.sun.jersey.core.header.FormDataContentDisposition;

/**
 * @version 1.0.0 2018-01-04
 * @author <a href="mailto:martynas@idega.is">Martynas Stakė</a>
 */
@Service
@Qualifier(PortalConstants.QUALIFIER_USER)
public class UserServiceImpl extends DefaultSpringBean implements UserService {

	@Override
	@Transactional(readOnly = true)
	public UserProfile getCitizenProfile(Filter filter, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		UserProfile profile = new UserProfile();
		User user = null;
		try {
			//Get authorized user
			user = SecurityUtil.getInstance().getAuthorizedUser(new IWContext(request, response, context));
			if (user == null) {
				return profile;
			}

			//Get the data to load from filter
			DataElement[] dataToLoadArray = null;
			if (filter != null && !ListUtil.isEmpty(filter.getDataToLoad())) {
				dataToLoadArray = filter.getDataToLoad().toArray(new DataElement[filter.getDataToLoad().size()]);
			}

			//Get the profile data
			profile = new UserProfile(user, getCurrentLocale(), dataToLoadArray);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting " + (user == null ? "citizen's" : user.getName() + " (personal ID: " + user.getPersonalID() + ")") + " profile", e);
		}
		return profile;
	}

	@Override
	public Result setProfile(UserProfile profile, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));
		Result result = new Result(Boolean.FALSE.toString(), iwrb.getLocalizedString("citizen_profile.please_provide_data", "Please provide data"));

		if (profile == null) {
			return result;
		}

		String errors = CoreConstants.EMPTY;
		try {
			User user = SecurityUtil.getInstance().getAuthorizedUser(new IWContext(request, response, context));
			if (user == null) {
				return result;
			}

			List<DataElement> dataToLoad = null;
			if (profile.getFilter() == null || ListUtil.isEmpty(profile.getFilter().getDataToLoad())) {
				dataToLoad = Arrays.asList(DataElement.values());
			} else {
				dataToLoad = profile.getFilter().getDataToLoad();
			}

			//Get the IDO user
			com.idega.user.data.User userIDO = null;
			String userPersonalId = CoreConstants.EMPTY;
			if (profile != null && !StringUtil.isEmpty(profile.getPersonalID())) {
				userPersonalId = profile.getPersonalID();
				userIDO = getUserByPersonalId(userPersonalId);
			} else {
				userIDO = getLegacyUser(user);
			}

			//Change the user data
			if (userIDO != null) {
				//**** Update the main user data ****
				//Name
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.GENERAL)) {
					if (!StringUtil.isEmpty(profile.getName())) {
						userIDO.setFullName(profile.getName());
						userIDO.store();
					}
				}

				//**** Change member password *****
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.LOGIN)) {
					if (!StringUtil.isEmpty(profile.getNewPassword()) && !StringUtil.isEmpty(profile.getNewPasswordRepeat())) {
						try {
							LoginTable loginTable = LoginDBHandler.getUserLogin(((Integer) userIDO.getPrimaryKey()).intValue());
							String passwordVerificationErrors = verifyPassword(loginTable, profile);
							if (StringUtil.isEmpty(passwordVerificationErrors)) {
								LoginDBHandler.updateLogin(((Integer) userIDO.getPrimaryKey()).intValue(), loginTable.getUserLogin(), profile.getNewPassword());
								LoginInfo info = LoginDBHandler.getLoginInfo(loginTable);
								info.store();
							} else {
								errors += passwordVerificationErrors;
							}
						} catch (Exception e1) {
							getLogger().log(Level.WARNING, "Could not change the user password for member with personal id: " + userPersonalId, e1);
							String errMsg = iwrb.getLocalizedString("user_update.could_not_change_password.error", "Could not change the user password for user with personal id: {0}. \n");
							errors += MessageFormat.format(errMsg, userPersonalId);
						}
					}
				}

				//Emails
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.EMAIL)) {
					try {
						//Validate emails
						List<String> validEmailsList = new ArrayList<String>();
						if (!StringUtil.isEmpty(profile.getEmail())) {
							String[] emailsArray = profile.getEmail().replace(CoreConstants.SPACE, CoreConstants.EMPTY).split(CoreConstants.COMMA);
							if (emailsArray != null) {
								List<String> emailsList = Arrays.asList(emailsArray);
								if (!ListUtil.isEmpty(emailsList)) {
									for (String emailIn : emailsList) {
										if (EmailValidator.getInstance().validateEmail(emailIn)) {
											validEmailsList.add(emailIn);
										} else {
											getLogger().log(Level.WARNING, "Invalid email: " + emailIn + " for member with personal id: " + userPersonalId);
											String errMsg = iwrb.getLocalizedString("user_update.invalid_email.error", "Invalid email: {0} was not saved. \n");
											errors += MessageFormat.format(errMsg, emailIn);
										}
									}
								}
							}
						}

						//Save emails
						EMailDAO emailDAO = ELUtil.getInstance().getBean(EMailDAO.class);
						ContactDAO contactDAO = ELUtil.getInstance().getBean(ContactDAO.class);
						EmailType emailTypeMain = contactDAO.getMainEmailType();
						//Remove all emails
						emailDAO.removeAllByUserId(Integer.valueOf(userIDO.getPrimaryKey().toString()));
						//Create emails
						if (!ListUtil.isEmpty(validEmailsList)) {
							for (int i = 0; i < validEmailsList.size(); i++) {
								emailDAO.createEmail(Integer.valueOf(userIDO.getPrimaryKey().toString()), validEmailsList.get(i), emailTypeMain);
							}
						}
					} catch (Exception eEmails) {
						getLogger().log(Level.WARNING, "Could not change the user emails for user with personal id: " + userPersonalId, eEmails);
						String errMsg = iwrb.getLocalizedString("user_update.could_not_change_emails.error", "Could not change the user emails for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//Phones
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.PHONE)) {
					try {
						//Get phones from string and validate them
						List<String> validPhonesList = new ArrayList<String>();
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
											String errMsg = iwrb.getLocalizedString("user_update.invalid_phone.error", "Invalid phone: {0} was not saved. \n");
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
						String errMsg = iwrb.getLocalizedString("user_update.could_not_change_phones.error", "Could not change the user phones for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
					}
				}

				//Address
				if (dataToLoad.contains(DataElement.ALL) || dataToLoad.contains(DataElement.ADDRESS)) {
					try {
						if (profile.getAddress() != null) {
							AddressBusiness addressBusiness = getServiceInstance(AddressBusiness.class);
							com.idega.core.location.data.Address address = userIDO.getUsersMainAddress();
							Country country = addressBusiness.getCountryHome().findByCountryName(profile.getAddress().getCountry());
							com.idega.core.location.data.PostalCode pc = null;
							if (profile.getAddress().getPostalCodeId() != null) {
								pc = addressBusiness.getPostalCodeHome().findByPrimaryKey(profile.getAddress().getPostalCodeId());
							} else {
								pc = addressBusiness.getPostalCodeAndCreateIfDoesNotExist(profile.getAddress().getPostalCode(), profile.getAddress().getCity(), country);
							}
							String streetName = addressBusiness.getStreetNameFromAddressString(profile.getAddress().getStreetAddress());
							String streetNumber = addressBusiness.getStreetNumberFromAddressString(profile.getAddress().getStreetAddress());
							if (address != null) {
								address.setStreetName(streetName);
								address.setStreetNumber(streetNumber);
								address.setPostalCode(pc);
								address.setCity(profile.getAddress().getCity());
								address.store();
							} else {
								UserBusiness userBusiness = getServiceInstance(UserBusiness.class);
								userBusiness.updateUsersMainAddressOrCreateIfDoesNotExist(
										userIDO,
										profile.getAddress().getStreetAddress(),
										pc,
										country,
										profile.getAddress().getCity(),
										null,
										null,
										null
								);
							}
						}
					} catch (Exception eAddr) {
						getLogger().log(Level.WARNING, "Could not update the user address for user with personal id: " + userPersonalId, eAddr);
						String errMsg = iwrb.getLocalizedString("user_update.could_not_update_user_address.error", "Could not update the user address for user with personal id: {0}. \n");
						errors += MessageFormat.format(errMsg, userPersonalId);
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
			}
		} catch (Exception e) {
			String errorMsg = "Error updating user: " + e.getLocalizedMessage();
			getLogger().log(Level.WARNING, errorMsg, e);
			errors += errorMsg;
			errors += " \n";
		}

		//Create return result
		if (StringUtil.isEmpty(errors)) {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.TRUE.toString());
		} else {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.FALSE.toString());
			result.setValue(errors);
		}

		return result;
	}

	private com.idega.user.data.User getUserByPersonalId(String personalId) {
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
					errors.append(iwrb.getLocalizedString("password_verification.new_and_repeat_passwords_different.error", "New user's password and repeat password are not the same. \n"));
				}
			}
			if (!StringUtil.isEmpty(profile.getNewPassword())) {
				if (profile.getNewPassword().length() < PortalConstants.MIN_PASSWORD_LENGTH) {
					String errMsg = iwrb.getLocalizedString("password_verification.new_password_too_short.error", "New user's password is too short. It should be at least {0} symbols length. \n");
					errors.append(MessageFormat.format(errMsg, String.valueOf(PortalConstants.MIN_PASSWORD_LENGTH)));
				}
			}
		} else {
			errors.append(iwrb.getLocalizedString("password_verification.could_not_find_login.error", "Could not find the login for the given user. \n"));
		}

		return errors.toString();
	}

	@Override
	public Result setProfilePicture(InputStream stream, FormDataContentDisposition info, String personalId, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));
		Result result = new Result(Boolean.FALSE.toString(), iwrb.getLocalizedString("citizen_profile.please_provide_data", "Please provide data"));

		if (stream == null || info == null) {
			return result;
		}

		com.idega.core.user.data.User userIDO = null;
		boolean success = true;
		try {
			User user = SecurityUtil.getInstance().getAuthorizedUser(new IWContext(request, response, context));
			if (user == null) {
				return result;
			}

			//Get the user
			if (StringUtil.isEmpty(personalId)) {
				userIDO = getLegacyUser(user);
			} else {
				userIDO = getUserByPersonalId(personalId);
			}
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting " + (userIDO == null ? "citizen's" : userIDO.getName() + " (personal ID: " + userIDO.getPersonalID() + ")"), e);
			success = false;
		}

		Integer fileId = null;
		if (userIDO == null) {
			success = false;
		} else {
			fileId = saveImage(stream, info);
			if (fileId != null) {
				userIDO.setSystemImageID(fileId);
				userIDO.store();
			}
		}

		//Create return result
		if (success) {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.TRUE.toString());
			if (fileId != null) {
				result.setValue(String.valueOf(fileId));
			}
		} else {
			result.setStatus(Status.OK.getStatusCode());
			result.setName(Boolean.FALSE.toString());
			result.setValue(iwrb.getLocalizedString("can_no_save_profile_picture", "Could not save the user picture."));
		}

		return result;
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
				file.store();
				Integer fileId = ((Integer) file.getPrimaryKey()).intValue();
				return fileId;
			} catch (Exception e) {
				getLogger().log(Level.WARNING, "Could not save the picture/image: " + e.getLocalizedMessage(), e);
			}
		}
		return null;
	}

	private Integer saveImage(InputStream stream, FormDataContentDisposition info) {
		if (stream != null && info != null && !StringUtil.isEmpty(info.getFileName())) {
			try {
				InputStream newStream = new ByteArrayInputStream(IOUtils.toByteArray(stream));

				String mimeType = "image/" + info.getFileName().substring(info.getFileName().lastIndexOf(CoreConstants.DOT) + 1);
				ICFile file = ((com.idega.core.file.data.ICFileHome) com.idega.data.IDOLookup.getHome(ICFile.class)).create();
				file.setName(info.getFileName());
				file.setCreationDate(IWTimestamp.getTimestampRightNow());
				file.setMimeType(mimeType);
				file.setFileValue(newStream);
				file.store();
				Integer fileId = ((Integer) file.getPrimaryKey()).intValue();
				return fileId;
			} catch (Exception e) {
				getLogger().log(
						Level.WARNING,
						"Could not save the picture/image: "
								+ e.getLocalizedMessage(), e);
			} finally {
				IOUtil.close(stream);
			}
		}
		return null;
	}

}