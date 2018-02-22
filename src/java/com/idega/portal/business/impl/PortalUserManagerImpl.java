package com.idega.portal.business.impl;

import java.util.Date;
import java.util.logging.Level;

import javax.ws.rs.core.Response;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.login.bean.OAuthToken;
import com.idega.block.login.business.OAuth2Service;
import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.core.accesscontrol.dao.UserLoginDAO;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.accesscontrol.data.LoginTableHome;
import com.idega.core.accesscontrol.data.bean.LoginRecord;
import com.idega.core.accesscontrol.data.bean.UserLogin;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials.LoginType;
import com.idega.core.business.DefaultSpringBean;
import com.idega.data.IDOLookup;
import com.idega.portal.business.PortalUserManager;
import com.idega.portal.model.Result;
import com.idega.presentation.IWContext;
import com.idega.user.dao.UserDAO;
import com.idega.user.data.bean.User;
import com.idega.util.CoreUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.RequestUtil;
import com.idega.util.StringUtil;
import com.idega.util.WebUtil;
import com.idega.util.expression.ELUtil;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class PortalUserManagerImpl extends DefaultSpringBean implements PortalUserManager {

	@Autowired
	private WebUtil webUtil;

	@Autowired
	private OAuth2Service oauth2Service;

	@Override
	public AccessToken getAccessToken(String uuid, String clientId) {
		try {
			if (StringUtil.isEmpty(uuid)) {
				getLogger().warning("UUID is not provided");
				return new AccessToken();
			}

			String userName = getLoginNameByUUID(uuid);
			if (StringUtil.isEmpty(userName)) {
				getLogger().warning("Unable to get user's login name by UUID: " + uuid);
				return new AccessToken();
			}

			if (StringUtil.isEmpty(clientId)) {
				clientId = oauth2Service.getDefaultClientId();
				if (StringUtil.isEmpty(clientId)) {
					getLogger().warning("Failed to resolve default client ID: " + clientId + " for user name: " + userName);
				} else {
					getLogger().info("Resolved default client ID: " + clientId + " for user name: " + userName);
				}
			}

			IWContext iwc = CoreUtil.getIWContext();
			LoginTableHome loginTableHome = (LoginTableHome) IDOLookup.getHome(LoginTable.class);
			LoginTable userLogin = loginTableHome.findByLogin(userName);
			LoggedInUserCredentials credentials = new LoggedInUserCredentials(
					iwc.getRequest(),
					RequestUtil.getServerURL(iwc.getRequest()),
					userName,
					userLogin.getUserPassword(),
					LoginType.AUTHENTICATION_GATEWAY,
					(Integer) userLogin.getPrimaryKey()
			);
			OAuthToken token = oauth2Service.getToken(clientId, credentials);
			if (token == null) {
				getLogger().warning("Unable to get OAuth token for user name: '" + userName + "', UUID: " + uuid);
				return new AccessToken();
			}

			getLogger().info("Created new token (" + token + ", refresh token: " + token.getRefresh_token() + ") for user name: '" + userName + "', UUID: " + uuid + " and client ID: " + clientId);
			return new AccessToken(token, userName);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting access token for UUID: " + uuid, e);
			return new AccessToken();
		}
	}

	@Override
	public Result logout(User user) {
		if (user == null) {
			return new Result(Response.Status.OK.getStatusCode(), Boolean.TRUE.toString());
		}

		try {
			webUtil.logOut();
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error logging out: " + user, e);
		}
		return new Result(Response.Status.OK.getStatusCode(), Boolean.TRUE.toString());
	}

	private String getLoginNameByUUID(String uuid) {
		if (StringUtil.isEmpty(uuid)) {
			getLogger().warning("UUID is not provided");
			return null;
		}

		UserDAO userDAO = ELUtil.getInstance().getBean(UserDAO.class);
		User user = userDAO.getUserByUUID(uuid);
		if (user == null) {
			getLogger().warning("Unable to find user by UUID: " + uuid);
			return null;
		}

		UserLoginDAO userLoginDAO = ELUtil.getInstance().getBean(UserLoginDAO.class);
		LoginRecord lRecord = userLoginDAO.getLastRecordByUser(user);
		if (lRecord == null) {
			getLogger().warning("Unable to find login record for " + user);
			return null;
		}

		Date loggedInAt = lRecord.getInStamp();
		if (loggedInAt == null) {
			getLogger().warning("Do not know when " + user + " logged in");
			return null;
		}

		long timePassedAfterLogin = IWTimestamp.RightNow().getTime().getTime() - loggedInAt.getTime();
		int allowedTimeOut = getApplication().getSettings().getInt("oauth_login_allowed_timeout", 30000);
		if (timePassedAfterLogin > allowedTimeOut) {
			getLogger().warning("It passed " + timePassedAfterLogin + " ms after " + user + " last time logged in, while it is allowed pass " + allowedTimeOut + " ms: disabling access");
			return null;
		}

		UserLogin login = userLoginDAO.findLoginForUser(user);
		if (login == null) {
			getLogger().warning("Unable to get login name for " + user);
			return null;
		}

		String userLogin = login.getUserLogin();
		getLogger().info("Found user login '" + userLogin + "' for UUID: " + uuid);
		return userLogin;
	}

}