package com.idega.portal.business.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Level;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.login.bean.OAuthToken;
import com.idega.block.login.business.OAuth2Service;
import com.idega.block.login.business.PasswordTokenBusiness;
import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.core.accesscontrol.business.LoginBusinessBean;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.accesscontrol.data.LoginTableHome;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials;
import com.idega.core.accesscontrol.event.LoggedInUserCredentials.LoginType;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.data.Email;
import com.idega.data.IDOLookup;
import com.idega.portal.business.PortalUserManager;
import com.idega.portal.model.Result;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
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

	@Autowired
	private PasswordTokenBusiness passwordTokenBusiness;

	@Override
	public AccessToken getAccessToken(String uuid, String token, String clientId, String type, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		try {
			if (StringUtil.isEmpty(uuid)) {
				getLogger().warning("UUID is not provided");
				return new AccessToken();
			}

			if (StringUtil.isEmpty(token)) {
				getLogger().warning("Token is not provided");
				return new AccessToken();
			}

			String userName = getLoginNameByUUID(uuid);
			if (StringUtil.isEmpty(userName)) {
				getLogger().warning("Unable to get user's login name by UUID: '" + uuid + "'");
				return new AccessToken();
			}

			boolean authorized = false;
			String adminToken = getSettings().getProperty("portal.admin_token");
			if (!StringUtil.isEmpty(adminToken) && adminToken.equals(token)) {
				authorized = true;
			}

			if (!authorized && passwordTokenBusiness.isTokenValid(token)) {
				authorized = true;
			}

			IWContext iwc = new IWContext(request, response, context);

			if (!authorized) {
				try {
					webUtil.logOff(iwc);
				} catch (Exception e) {}

				getLogger().warning("Token '" + token + "' is not valid anymore");
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
			OAuthToken oAuthToken = oauth2Service.getToken(clientId, credentials);
			if (oAuthToken == null) {
				getLogger().warning("Unable to get OAuth token for user name: '" + userName + "', UUID: " + uuid);
				return new AccessToken();
			}

			getLogger().info("Created new token (" + oAuthToken + ", refresh token: " + oAuthToken.getRefresh_token() + ") for user name: '" + userName + "', UUID: " + uuid +
					", token: " + token + ", client ID: " + clientId + " and type: " + type);

			if (StringUtil.isEmpty(type)) {
				getLogger().warning("Unknown login type for UUID " + uuid);
			} else {
				type = type.concat(CoreConstants.AT).concat(LoginType.AUTHENTICATION_GATEWAY.toString());
				iwc.setSessionAttribute(LoggedInUserCredentials.LOGIN_TYPE, type);
			}

			if (getSettings().getBoolean("portal.publish_event_for_token", false)) {
				User user = getUser(userLogin.getUser());
				LoginBusinessBean loginBusiness = LoginBusinessBean.getLoginBusinessBean(iwc);
				loginBusiness.doPublishLoggedInEvent(request, response, context, user, userName, type);
			}

			return new AccessToken(oAuthToken, userName);
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

	private String getEmailAddress(com.idega.user.data.User user) {
		if (user == null) {
			return null;
		}

		try {
			Collection<Email> emails = user.getEmails();
			if (ListUtil.isEmpty(emails)) {
				return null;
			}

			String emailAddress = null;
			for (Iterator<Email> iter = emails.iterator(); (iter.hasNext() && StringUtil.isEmpty(emailAddress));) {
				Email email = iter.next();
				emailAddress = email == null ? null : email.getEmailAddress();
			}
			return emailAddress;
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting email for " + user, e);
		}

		return null;
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
		} catch (Exception e) {}
		if (user == null) {
			getLogger().warning("Unable to find user by UUID: '" + uuid + "'");
			return null;
		}

		LoginTable login = LoginDBHandler.getUserLogin(user);
		if (login == null) {
			try {
				String username = user.getPersonalID();
				if (getSettings().getBoolean("portal.email_as_username", false)) {
					String email = getEmailAddress(user);
					username = StringUtil.isEmpty(email) ? username : email;
				}

				login = LoginDBHandler.createLogin(
						user,
						username,
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
						"Failed creating login for user " + user + " with uuid='" + uuid + "'",
						e
				);
			}
			if (login == null) {
				getLogger().warning("Unable to create login for " + user + " with uuid='" + uuid + "'");
				return null;
			}
		}

		String userLogin = login.getUserLogin();
		return userLogin;
	}

}