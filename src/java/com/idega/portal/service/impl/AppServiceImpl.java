package com.idega.portal.service.impl;

import java.util.logging.Level;

import javax.ws.rs.core.Response.Status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.accesscontrol.business.LoginBusinessBean;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.business.DefaultSpringBean;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.service.AppService;
import com.idega.portal.service.PortalService;
import com.idega.user.data.User;
import com.idega.util.CoreConstants;
import com.idega.util.StringUtil;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Qualifier(PortalConstants.QUALIFIER_APP)
public class AppServiceImpl extends DefaultSpringBean implements AppService {

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_PORTAL)
	private PortalService portalService;

	@Override
	public PortalSettings getDashboardSettings() {
		return portalService.getDashboardSettings();
	}

	@Override
	public String logout(){
		return portalService.logout();
	}

	@Override
	public Result isValidLogin(UserAccount credentials) {
		Result result = new Result(Status.BAD_REQUEST.getStatusCode(), Boolean.FALSE.toString());
		if (credentials == null) {
			return result;
		}

		String username = credentials.getUsername();
		String password = credentials.getPassword();
		if (StringUtil.isEmpty(username) || StringUtil.isEmpty(password)) {
			return result;
		}

		try {
			LoginTable loginTable = LoginDBHandler.getUserLoginByUserName(username);
			if (loginTable == null) {
				return result;
			}

			User user = loginTable.getUser();
			if (user == null) {
				return result;
			}

			LoginBusinessBean loginBusiness = LoginBusinessBean.getLoginBusinessBean(getIWApplicationContext());
			if (loginBusiness.verifyPassword(getUser(user), username, password)) {
				result.setStatus(Status.OK.getStatusCode());
				result.setName(Boolean.TRUE.toString());
				return result;
			}
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error verifying: " + username + CoreConstants.SLASH + password, e);
		}

		return result;
	}

}