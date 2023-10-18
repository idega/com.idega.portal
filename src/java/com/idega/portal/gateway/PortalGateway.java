package com.idega.portal.gateway;

import com.idega.io.DownloadWriter;
import com.idega.portal.service.PortalService;
import com.idega.portal.service.UserService;

public interface PortalGateway extends PortalService, UserService {

	public static final String	PATH = "/api/portal/",

								PORTAL = "/portal",
								SETTINGS = "/settings",
								ACCOUNT = "/account",
								MENUS = "/menus",
								LANGUAGE = "/language",
								LOCALIZE = "/localize",
								STRINGS = "/strings",
								AVAILABLE = "/available",
								ADD = "/add",
								REMOVE = "/remove",

								LOCALIZE_ARTICLES = LANGUAGE + LOCALIZE + "/articles",

								LOGOUT = "/logout",
								LOGIN = "/login",

								AUTHORIZE = "/authorize",
								UNAUTHORIZE = "/unauthorize",

								PING = "/ping",

								USER = "/user",
								PROFILE = "/profile",
								AGREEMENT = "/agreement",
								MESSAGES = "/messages",
								UPDATE = "/update",
								PICTURE = "/picture",
								REMIND_PASSWORD = "/remindpassword",
								PASSWORD_TOKEN = "/password_token",
								PASSWORD_TOKEN_BY_ID = PASSWORD_TOKEN + "/{tokenId}",
								PASSWORD_TOKEN_PASSWORD = PASSWORD_TOKEN_BY_ID + "/password",

								UPLOAD_FILES = "/files/upload",
								FILE = "/file/{identifier}/{" + DownloadWriter.PRM_FILE_TOKEN + "}",

								ARTICLE = "/article",
								ARTICLES = "/articles",
								ARTICLE_LOCALIZED = ARTICLE + "/{language}",
								LOCALIZED_ARTICLE = "/localized-article";

}