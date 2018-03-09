package com.idega.portal.security;

import java.util.List;

import com.idega.presentation.IWContext;
import com.idega.user.data.bean.User;

public interface SecurityResolver {

	public boolean hasAccess(IWContext iwc, User user, List<String> roles);

}