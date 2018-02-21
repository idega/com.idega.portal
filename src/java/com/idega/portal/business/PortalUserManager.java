package com.idega.portal.business;

import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.portal.model.Result;
import com.idega.user.data.bean.User;

public interface PortalUserManager {

	public AccessToken getAccessToken(String uuid, String clientId);

	public Result logout(User user);

}