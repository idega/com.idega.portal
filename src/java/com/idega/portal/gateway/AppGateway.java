package com.idega.portal.gateway;

import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.portal.service.AppService;

public interface AppGateway extends AppService {

	public static final String	PATH = "/app",
								ACCESS_TOKEN = "/token";

	public AccessToken getAccessToken(String uuid, String clientId);

}