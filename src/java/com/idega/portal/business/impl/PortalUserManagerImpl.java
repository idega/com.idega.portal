package com.idega.portal.business.impl;

import java.util.logging.Level;

import javax.ws.rs.core.Response;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.login.bean.OAuthToken;
import com.idega.block.login.business.OAuth2Service;
import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.core.accesscontrol.business.LoginBusinessBean;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.accesscontrol.data.LoginTableHome;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials.LoginType;
import com.idega.core.business.DefaultSpringBean;
import com.idega.data.IDOLookup;
import com.idega.portal.business.PortalUserManager;
import com.idega.portal.model.Result;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.RequestUtil;
import com.idega.util.StringUtil;
import com.idega.util.WebUtil;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class PortalUserManagerImpl extends DefaultSpringBean implements PortalUserManager {

	@Autowired
	private WebUtil webUtil;

	@Autowired
	private OAuth2Service oauth2Service;

	@Override
	public AccessToken getAccessToken(String uuid, String clientId, String type) {
		try {
			if (StringUtil.isEmpty(uuid)) {
				getLogger().warning("UUID is not provided");
				return new AccessToken();
			}

			String userName = getLoginNameByUUID(uuid);
			if (StringUtil.isEmpty(userName)) {
				getLogger().warning("Unable to get user's login name by UUID: '" + uuid + "'");
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
					(Integer) userLogin.getPrimaryKey(),
					type
			);
			OAuthToken token = oauth2Service.getToken(clientId, credentials);
			if (token == null) {
				getLogger().warning("Unable to get OAuth token for user name: '" + userName + "', UUID: " + uuid);
				return new AccessToken();
			}

			getLogger().info("Created new token (" + token + ", refresh token: " + token.getRefresh_token() + ") for user name: '" + userName + "', UUID: " + uuid + ", client ID: " + clientId + " and type: " + type);

			if (StringUtil.isEmpty(type)) {
				getLogger().warning("Unknown login type for UUID " + uuid);
			} else {
				type = type.concat(CoreConstants.AT).concat(LoginType.AUTHENTICATION_GATEWAY.toString());
				iwc.setSessionAttribute(LoggedInUserCredentials.LOGIN_TYPE, type);
			}

			if (getSettings().getBoolean("portal.publish_event_for_token", false)) {
				User user = getUser(userLogin.getUser());
				LoginBusinessBean loginBusiness = LoginBusinessBean.getLoginBusinessBean(iwc);
				loginBusiness.doPublishLoggedInEvent(iwc.getRequest(), user, userName, type);
			}

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

		UserBusiness userBusiness = getServiceInstance(UserBusiness.class);
		com.idega.user.data.User user = null;
		try {
			user = userBusiness.getUserByUniqueId(uuid);
		} catch (Exception e) {
			getLogger().log(
					Level.WARNING, 
					"Failed finding user by unique id '" + uuid +"'",
					e
			);
		}
		if (user == null) {
			getLogger().warning("Unable to find user by UUID: '" + uuid + "'");
			return null;
		}

		LoginTable login = LoginDBHandler.getUserLogin(user);
		if (login == null) {
			getLogger().warning("Creating login for " + user);
			try {
				login = LoginDBHandler.createLogin(
						user, 
						user.getPersonalID(), 
						user.getUniqueId(), 
						Boolean.TRUE, 
						IWTimestamp.RightNow(), 
						-1, 
						Boolean.FALSE, 
						Boolean.TRUE, 
						Boolean.FALSE, 
						null
				);
			} catch (Exception e) {
				getLogger().log(
						Level.WARNING, 
						"Failed creating login for user uuid='" + uuid +"'",
						e
				);
			}
			if (login == null) {
				getLogger().warning("Unable to create user login by uuid='"+uuid+"'");
				return null;
			}
		}

		String userLogin = login.getUserLogin();
		return userLogin;
	}

}