package com.idega.portal.gateway;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.portal.service.AppService;

public interface AppGateway extends AppService {

	public static final String	PATH = "/app",
								ACCESS_TOKEN = "/token",
								CREDENTIALS = "/credentials";

	public AccessToken getAccessToken(String uuid, String clientId, String type, HttpServletRequest request, HttpServletResponse response, ServletContext context);

}