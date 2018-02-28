package com.idega.portal.gateway;

import com.idega.portal.service.PortalService;
import com.idega.portal.service.UserService;

public interface PortalGateway extends PortalService, UserService {

	public static final String	PATH = "/api/portal/",

								PORTAL = "/portal",
								SETTINGS = "/settings",
								ACCOUNT = "/account",
								MENUS = "/menus",
								LANGUAGE = "/language",

								LOGGOUT = "/loggout",

								AUTHORIZE = "/authorize",

								USER = "/user",
								PROFILE = "/profile",
								MESSAGES = "/messages",
								UPDATE = "/update",
								PICTURE = "/picture",
								REMIND = "/remind",
								PASSWORD = "/password";

}