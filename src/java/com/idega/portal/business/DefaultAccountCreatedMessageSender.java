package com.idega.portal.business;

import java.util.Locale;
import java.util.logging.Level;

import com.idega.block.process.message.business.MessageBusiness;
import com.idega.core.business.DefaultSpringBean;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.SendMail;
import com.idega.util.StringUtil;

public class DefaultAccountCreatedMessageSender extends DefaultSpringBean implements MessageSender {

	@Override
	public void sendUserMessages(
			User user,
			Locale locale
	) throws Exception {
		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		IWBundle bundle = iwma.getBundle(getBundleIdentifier());
		IWResourceBundle iwrb = bundle.getResourceBundle(locale);
		String subject = getSubject(user, iwrb);
		String body = getBody(user, iwrb);
		sendUserMessages(user, subject, body);
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
			IWResourceBundle iwrb
	) {
		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		IWMainApplicationSettings settings = iwma.getSettings();
		Object[] paremeters = {user.getDisplayName()};
		String body = iwrb.getLocalizedAndFormattedString(
				"message.email.account_created.body",
				"Hi {0}.\n\nRegistration completed. Your account is already active, so you can log in and use your social security number as the username and password you chose yourself.",
				paremeters
		);
		String team = iwrb.getLocalizedString(
				"message.email.account_created.body.with_regards",
				null
		);
		if(StringUtil.isEmpty(team)) {
			team = settings.getProperty("with_regards_text", "Idega");
		}
		body += "\n\n" + team;
		return body;
	}

	protected String getBundleIdentifier() {
		return PortalConstants.IW_BUNDLE_IDENTIFIER;
	}

	protected void sendUserMessages(
			User user,
			String subject,
			String body
	) throws Exception {
		try {
			MessageBusiness messageBusiness = getServiceInstance(MessageBusiness.class);
			messageBusiness.createUserMessage(null, getLegacyUser(user), subject, body, false);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error creating message for " + user, e);
		}

		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		IWMainApplicationSettings settings = iwma.getSettings();
		String emailTo = user.getEmailAddress();
		String from = settings.getProperty(CoreConstants.PROP_SYSTEM_MAIL_FROM_ADDRESS, "staff@idega.com");
		SendMail.send(from, emailTo, null, null, null, null, subject, body);
	}

}