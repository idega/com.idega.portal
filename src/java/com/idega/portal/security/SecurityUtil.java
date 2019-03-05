package com.idega.portal.security;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Path;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.context.support.WebApplicationContextUtils;

import com.idega.block.login.business.OAuth2Service;
import com.idega.block.sso.model.AuthorizationSettings;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.cache.IWCacheManager2;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.gateway.Gateway;
import com.idega.portal.model.Property;
import com.idega.portal.security.model.AccessArtifact;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.RequestResponseProvider;
import com.idega.user.data.bean.User;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.datastructures.map.MapUtil;
import com.idega.util.expression.ELUtil;

public class SecurityUtil {

	private static final Logger LOGGER = Logger.getLogger(SecurityUtil.class.getName());

	private static final SecurityUtil instance = new SecurityUtil();

	private static Map<String, AccessArtifact> ALL_ARTIFACTS = new HashMap<>();

	private static Set<String> ALL_ROLES = new HashSet<>();

	@Autowired
	private List<Gateway> beans;
	
	@Autowired
	private OAuth2Service oauthService;

	private SecurityUtil() {}

	private OAuth2Service getOAuth2Service() {
		if (oauthService == null) {
			ELUtil.getInstance().autowire(this);
		}
		return oauthService;
	}

	private List<Gateway> getGateways() {
		if (this.beans == null) {
			ELUtil.getInstance().autowire(this);
		}

		return this.beans;
	}

	public static final SecurityUtil getInstance() {
		return instance;
	}

	public List<AccessArtifact> getAllAccessArtifacts() {
		return new ArrayList<>(ALL_ARTIFACTS.values());
	}

	public List<String> getAllRoles() {
		return new ArrayList<>(ALL_ROLES);
	}

	public void doRegisterAccessArtifact(String name, List<Property<String, List<String>>> accesses) {
		 if (StringUtil.isEmpty(name) || ListUtil.isEmpty(accesses)) {
			 return;
		 }

		 AccessArtifact artifact = ALL_ARTIFACTS.get(name);
		 if (artifact == null) {
			 artifact = new com.idega.portal.security.model.AccessArtifact(name, accesses);
			 ALL_ARTIFACTS.put(name, artifact);
		 }
	}

	public void doRegisterRoles(List<String> roles) {
		if (ListUtil.isEmpty(roles)) {
			return;
		}

		ALL_ROLES.addAll(roles);
	}

	public User getAuthorizedUser() {
		List<Gateway> gateways = getGateways();
		String methodName = getCurrentMethodName(gateways);
		String uri = getURI(methodName, gateways);
		if (StringUtil.isEmpty(uri)) {
			if (!StringUtil.isEmpty(methodName) && !ListUtil.isEmpty(gateways)) {
				LOGGER.warning("Did not find URI for " + methodName + " in " + gateways + ". WS: " + getRequestURI());
			}
			return null;
		}

		return getAuthorizedUser(uri);
	}

	private String getCurrentMethodName(List<Gateway> gateways) {
		if (ListUtil.isEmpty(gateways)) {
			LOGGER.warning("There are no gateways with type " + Gateway.class.getName());
			return null;
		}

		final StackTraceElement[] stElements = Thread.currentThread().getStackTrace();
		if (ArrayUtil.isEmpty(stElements)) {
			LOGGER.warning("Stack trace is not available");
			return null;
		}

		for (Gateway gateway: gateways) {
			if (gateway == null) {
				continue;
			}

			String gatewayClassName = gateway.getClass().getName();
			for (StackTraceElement ste: stElements) {
				if (ste == null) {
					continue;
				}

				String className = ste.getClassName();
				if (StringUtil.isEmpty(className)) {
					continue;
				}

				if (gatewayClassName.equals(className)) {
					return ste.getMethodName();
				}
			}
		}

		return null;
	}

	private String getURI(String methodName, List<Gateway> gateways) {
		if (StringUtil.isEmpty(methodName)) {
			return null;
		}

		if (ListUtil.isEmpty(gateways)) {
			LOGGER.warning("There are no gateways with type " + Gateway.class.getName());
			return null;
		}

		String uri = null;
		for (Iterator<Gateway> gatewaysIter = gateways.iterator(); (StringUtil.isEmpty(uri) && gatewaysIter.hasNext());) {
			uri = getURI(methodName, gatewaysIter.next());
		}

		return uri;
	}

	private String getURI(String methodName, Gateway gateway) {
		if (StringUtil.isEmpty(methodName) || gateway == null) {
			return null;
		}

		Method[] methods = gateway.getClass().getMethods();
		if (ArrayUtil.isEmpty(methods)) {
			return null;
		}

		Method m = null;
		for (Iterator<Method> methodsIter = Arrays.asList(methods).iterator(); (m == null && methodsIter.hasNext());) {
			m = methodsIter.next();
			if (!methodName.equals(m.getName())) {
				m = null;
			}
		}
		if (m == null) {
			LOGGER.warning("There is no method '" + methodName + "' in " + gateway.getClass().getName());
			return null;
		}

		Annotation pathAnnotation = m.getAnnotation(Path.class);
		String value = pathAnnotation == null ? null : pathAnnotation.toString();
		if (StringUtil.isEmpty(value)) {
			LOGGER.warning("Method '" + methodName + "' does not have annotation " + Path.class.getName() + " in " + gateway.getClass().getName());
			return null;
		}

		String start = CoreConstants.AT.concat(Path.class.getName()).concat(CoreConstants.BRACKET_LEFT).concat("value").concat(CoreConstants.EQ);
		if (value.startsWith(start)) {
			value = StringHandler.replace(value, start, CoreConstants.EMPTY);
		}
		if (value.endsWith(CoreConstants.BRACKET_RIGHT)) {
			value = value.substring(0, value.length() - 1);
		}

		value = StringHandler.replace(value, "{id}/", CoreConstants.EMPTY);	//	Removing path parameter

		return value;
	}

	public HttpServletRequest getRequest() {
		RequestResponseProvider requestProvider = null;
		try {
			requestProvider = ELUtil.getInstance().getBean(RequestResponseProvider.class);
			return requestProvider == null ? null : requestProvider.getRequest();
		} catch (Exception e) {}
		return null;
	}

	public String getRequestURI() {
		HttpServletRequest request = getRequest();
		return request == null ? null : request.getRequestURI();
	}

	public User getCurrentUser() {
		User user = null;
		try {
			user = getOAuth2Service().getAuthenticatedUser();
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Failed to get authinticated user, cause of:", e);
		}

		return user;
	}

	public User getAuthorizedUser(String uri) {
		User user = null;
		try {
			user = getCurrentUser();
			if (user == null) {
				LOGGER.warning("Not authorized for WS '" + uri + "' because not logged in");
				return null;
			}

			if (StringUtil.isEmpty(uri)) {
				return user;
			}

			IWContext iwc = CoreUtil.getIWContext();
			IWMainApplication iwma = iwc == null ? IWMainApplication.getDefaultIWMainApplication() : iwc.getIWMainApplication();

			Property<String, List<String>> roles = AccessArtifact.getRoles(iwma.getSettings(), uri);
			if (roles == null || ListUtil.isEmpty(roles.getValue())) {
				return user;
			}

			Map<String, Boolean> cachedAccess = IWCacheManager2.getInstance(iwma).getCache("dashboard.securityServices.getAuthorizedUser", 1800, true, false, 3000000);

			String key = roles.getName().concat(uri).concat(String.valueOf(user.getId()));
			if (cachedAccess.containsKey(key)) {
				return cachedAccess.get(key) ? user : null;
			}

			if (hasAnyRole(user, roles.getValue())) {
				cachedAccess.put(key, Boolean.TRUE);
				return user;
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting authorized user. URI: " + uri, e);
		}

		return null;
	}

	public boolean hasRole(User user, String role) {
		return StringUtil.isEmpty(role) ? false : hasAnyRole(user, Arrays.asList(role));
	}

	public boolean hasAnyRole(User user, List<String> roles) {
		if (user == null || ListUtil.isEmpty(roles)) {
			return false;
		}

		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			LOGGER.warning(IWContext.class.getName() + " is unavailable, unable to check is user " + user + " has role(s) " + roles);
			return false;
		}

		if (iwc.isSuperAdmin()) {
			return true;
		}

		Map<String, SecurityResolver> resolvers = WebApplicationContextUtils.getWebApplicationContext(iwc.getServletContext()).getBeansOfType(SecurityResolver.class);
		if (!MapUtil.isEmpty(resolvers)) {
			for (SecurityResolver resolver: resolvers.values()) {
				if (resolver.hasAccess(iwc, user, roles)) {
					return true;
				}
			}
		}

		AccessController accessController = iwc.getAccessController();

		Set<String> userRoles = accessController.getAllRolesForUser(user);
		if (!ListUtil.isEmpty(userRoles)) {
			for (String role: roles) {
				if (userRoles.contains(role)) {
					return true;
				}
			}
		}

		return false;
	}

	public List<String> getAllRoles(User user){
		if (user == null) {
			return null;
		}

		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			LOGGER.warning(IWContext.class.getName() + " is unavailable, unable to get user " + user + " role(s)");
			return null;
		}

		if (iwc.isSuperAdmin()) {
			return new ArrayList<>(ALL_ROLES);
		}

		AccessController accessController = iwc.getAccessController();

		List<String> result = new ArrayList<String>();

		Set<String> userRoles = accessController.getAllRolesForUser(user);
		if (!ListUtil.isEmpty(userRoles)) {
			Set<String> tmp = new HashSet<>(ALL_ROLES);
			for (String role: tmp) {
				if (userRoles.contains(role)) {
					result.add(role);
				}
			}
		}
		return result.size() < 1 ? null : result;
	}

	private IWMainApplicationSettings getSettings() {
		return IWMainApplication.getDefaultIWMainApplication().getSettings();
	}

	public List<AuthorizationSettings> getAllAuthorizationSettings() {
		return getAllAuthorizationSettings(null);
	}
	public List<AuthorizationSettings> getAllAuthorizationSettings(List<String> types) {
		List<AuthorizationSettings> allSettings = new ArrayList<>();

		if (ListUtil.isEmpty(types)) {
			String typesProp = getSettings().getProperty("dashboard.remote_login_types", "default");
			if (typesProp != null) {
				types = Arrays.asList(typesProp.split(CoreConstants.COMMA));
			}
		}

		types = ListUtil.isEmpty(types) ? Arrays.asList(CoreConstants.EMPTY) : types;

		for (String type: types) {
			allSettings.add(getAuthorizationSettings(type));
		}

		return allSettings;
	}

	public AuthorizationSettings getAuthorizationSettings(String type) {
		type = type == null ? CoreConstants.EMPTY : type;

		AuthorizationSettings settings = new AuthorizationSettings();
		settings.setType(type);

		IWMainApplicationSettings appSettings = getSettings();
		String propPrefix = "dashboard.remote" + (StringUtil.isEmpty(type) ? CoreConstants.EMPTY : CoreConstants.UNDER.concat(type));

		settings.setRemoteLoginService(appSettings.getProperty(propPrefix.concat("_login_service")));
		settings.setRemoteLoginEntityId(appSettings.getProperty(propPrefix.concat("_login_entity_id")));
		settings.setRemoteLoginReturn(appSettings.getProperty(propPrefix.concat("_login_return")));
		settings.setRemoteLoginTarget(appSettings.getProperty(propPrefix.concat("_login_target")));
		settings.setRemoteLoginLogo(appSettings.getProperty(propPrefix.concat("_login_logo")));

		String locProp = propPrefix.concat("_login_label");
		IWBundle bundle = IWMainApplication.getDefaultIWMainApplication().getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER);
		if (bundle != null) {
			IWContext iwc = CoreUtil.getIWContext();
			Locale locale = iwc == null ? CoreUtil.getCurrentLocale() : null;
			if (iwc != null || locale != null) {
				IWResourceBundle iwrb = iwc == null ? bundle.getResourceBundle(locale) : bundle.getResourceBundle(iwc);
				settings.setRemoteLoginLabel(iwrb.getLocalizedString(locProp, locProp));
			}
		}
		return settings;
	}

}