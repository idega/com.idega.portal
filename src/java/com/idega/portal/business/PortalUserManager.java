package com.idega.portal.business;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.portal.model.Result;
import com.idega.user.data.bean.User;

public interface PortalUserManager {

	public AccessToken getAccessToken(String uuid, String token, String clientId, String type, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result logout(User user);

}