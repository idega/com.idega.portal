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
import java.util.logging.Level;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.idega.core.business.DefaultSpringBean;
import com.idega.core.file.data.ICFile;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.business.UserProfileHelper;
import com.idega.portal.model.DataElement;
import com.idega.portal.model.Filter;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.UserService;
import com.idega.presentation.IWContext;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.IOUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.sun.jersey.api.client.ClientResponse.Status;
import com.sun.jersey.core.header.FormDataContentDisposition;

/**
 * @version 1.0.0 2018-01-04
 * @author <a href="mailto:martynas@idega.is">Martynas Stakė</a>
 */
@Service
@Qualifier(PortalConstants.QUALIFIER_USER)
public class UserServiceImpl extends DefaultSpringBean implements UserService {

	@Autowired
	private UserProfileHelper userProfileHelper;

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
		User user = null;
		try {
			IWContext iwc = new IWContext(request, response, context);
			user = SecurityUtil.getInstance().getAuthorizedUser(iwc);
			if (user == null) {
				return null;
			}

			return userProfileHelper.setProfile(iwc, profile, user);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error updating profile " + profile + " by + " + user, e);
		}
		return null;
	}

	@Override
	public Result setProfilePicture(
			InputStream stream,
			FormDataContentDisposition info,
			String personalId,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) {
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
				userIDO = userProfileHelper.getUserByPersonalId(personalId);
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
				file.setPublic(true);
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

	@Override
	public Result setUserReadAgreement(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		com.idega.user.data.User user = null;
		try {
			IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));
			Result result = new Result(Boolean.FALSE.toString(), iwrb.getLocalizedString("citizen_profile.eula_error", "Error"));

			User authorizedUser = SecurityUtil.getInstance().getAuthorizedUser(new IWContext(request, response, context));
			if (authorizedUser == null) {
				return result;
			}

			user = getLegacyUser(authorizedUser);
			user.setMetaData(PortalConstants.METADATA_EULA_AGREED, Boolean.TRUE.toString());
			user.store();

			return new Result(javax.ws.rs.core.Response.Status.OK.getStatusCode(), Boolean.TRUE.toString(), iwrb.getLocalizedString("citizen_profile.eula_success", "Success"));
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error marking " + user + " read agreement", e);
		}

		return null;
	}

}