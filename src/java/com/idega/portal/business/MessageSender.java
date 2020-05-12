package com.idega.portal.business;

import java.util.Locale;

import com.idega.user.data.User;

public interface MessageSender {

	void sendUserMessages(
			User user,
			Locale locale,
			boolean personalIdAsUserName,
			String emailAddress
	) throws Exception;

}