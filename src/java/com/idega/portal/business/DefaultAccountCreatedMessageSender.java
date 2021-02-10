package com.idega.portal.business;

import java.util.Locale;
import java.util.logging.Level;

import com.idega.block.process.message.business.MessageBusiness;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.data.Email;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.user.data.User;
import com.idega.util.CoreConstants;
import com.idega.util.EmailValidator;
import com.idega.util.SendMail;
import com.idega.util.StringUtil;

public class DefaultAccountCreatedMessageSender extends DefaultSpringBean implements MessageSender {

	@Override
	public void sendUserMessages(
			User user,
			Locale locale,
			boolean personalIdAsUserName,
			String emailAddress
	) throws Exception {
		IWBundle bundle = getBundle(getBundleIdentifier());
		IWResourceBundle iwrb = bundle.getResourceBundle(locale);
		String subject = getSubject(user, iwrb);
		String body = getBody(user, iwrb, personalIdAsUserName);
		sendUserMessages(user, subject, body, emailAddress);
	}

	protected String getSubject(
			User user,
			IWResourceBundle iwrb
	) {
		return iwrb.getLocalizedString(
				"message.email.account_created.subject",
				"Account was created"
		);
	}

	protected String getBody(
			User user,
			IWResourceBundle iwrb,
			boolean personalIdAsUserName
	) {
		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		IWMainApplicationSettings settings = iwma.getSettings();
		Object[] paremeters = {user.getDisplayName()};
		String body = null;
		if (personalIdAsUserName) {
			body = iwrb.getLocalizedAndFormattedString(
					"message.email.account_created.body",
					"Hi {0}.<br /><br />Registration completed. Your account is already active, so you can log in and use your social security number as the username and password you chose yourself.",
					paremeters
			);
		} else {
			body = iwrb.getLocalizedAndFormattedString(
					"message.email.account_created.body_user_name_as_email",
					"Hi {0}.<br /><br />Registration completed. Your account is already active, so you can log in with your user name and the password you chose yourself.",
					paremeters
			);
		}

		String team = iwrb.getLocalizedString(
				"message.email.account_created.body.with_regards",
				null
		);
		if (StringUtil.isEmpty(team)) {
			team = settings.getProperty("with_regards_text", null);
		}
		if (!StringUtil.isEmpty(team)) {
			body += "<br /><br />" + team;
		}
		return body;
	}

	protected String getBundleIdentifier() {
		return PortalConstants.IW_BUNDLE_IDENTIFIER;
	}

	protected void sendUserMessages(
			User user,
			String subject,
			String body,
			String emailAddress
	) throws Exception {
		try {
			MessageBusiness messageBusiness = getServiceInstance(MessageBusiness.class);
			messageBusiness.createUserMessage(null, user, subject, body, false);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error creating message for " + user, e);
		}

		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		IWMainApplicationSettings settings = iwma.getSettings();

		String emailTo = emailAddress;
		try {
			Email email = user.getUsersEmail();
			String address = email.getEmailAddress();
			emailTo = EmailValidator.getInstance().isValid(address) ? address : emailTo;
		} catch (Exception e) {}

		String from = settings.getProperty(CoreConstants.PROP_SYSTEM_MAIL_FROM_ADDRESS, "staff@idega.com");
		SendMail.send(from, emailTo, null, null, null, null, subject, body);
	}

}