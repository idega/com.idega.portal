package com.idega.portal.business;

import java.util.Locale;

import com.idega.user.data.bean.User;

public interface MessageSender {
	void sendUserMessages(
			User user,
			Locale locale
	) throws Exception;
}
