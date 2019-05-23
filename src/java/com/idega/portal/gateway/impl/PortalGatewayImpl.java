package com.idega.portal.gateway.impl;

import java.io.InputStream;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.idega.portal.PortalConstants;
import com.idega.portal.gateway.PortalGateway;
import com.idega.portal.model.Filter;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.model.UserProfile;
import com.idega.portal.service.PortalService;
import com.idega.portal.service.UserService;
import com.idega.restful.business.DefaultRestfulService;
import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

@Component
@Path(PortalGateway.PATH)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class PortalGatewayImpl extends DefaultRestfulService implements PortalGateway {

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_PORTAL)
	private PortalService portalService;

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_USER)
	private UserService userService;

	/* WS for dashboard */
	@Override
	@GET
	@Path(SETTINGS)
	public PortalSettings getDashboardSettings() {
		return portalService.getDashboardSettings();
	}

	@Override
	@POST
	@Path(ACCOUNT)
	public UserAccount doCreateAccount(UserAccount account) {
		return portalService.doCreateAccount(account);
	}

	@Override
	@GET
	@Path(MENUS)
	public List<PortalMenu> getPortalMenus() {
		return portalService.getPortalMenus();
	}

	@Override
	@GET
	@Path(AUTHORIZE)
	public String doAuthorizeViaGateway(
			@Context HttpServletRequest httpRequest,
			@Context HttpServletResponse httpResponse,
			@QueryParam("type") String type
	) {
		return portalService.doAuthorizeViaGateway(httpRequest, httpResponse, type);
	}

	@Override
	@GET
	@Path(UNAUTHORIZE)
	public String doUnAuthorizeViaGateway(
			@Context HttpServletRequest httpRequest,
			@Context HttpServletResponse httpResponse,
			@QueryParam("uri") String uri
	) {
		return portalService.doUnAuthorizeViaGateway(httpRequest, httpResponse, uri);
	}

	@Override
	@GET
	@Path(LANGUAGE)
	public String setLanguage(@QueryParam("language") String language) {
		return portalService.setLanguage(language);
	}

	@Override
	@POST
	@Path(LANGUAGE + LOCALIZE)
	public Result setLocalization(Localization localization) {
		return portalService.setLocalization(localization);
	}

	@Override
	@POST
	@Path(LANGUAGE + LOCALIZE + STRINGS)
	public Result setLocalizations(Localizations localizations) {
		return portalService.setLocalizations(localizations);
	}

	@Override
	@GET
	@Path(LANGUAGE + AVAILABLE)
	public List<LanguageData> getAvailableLanguages() {
		return portalService.getAvailableLanguages();
	}

	@Override
	@GET
	@Path(LANGUAGE + ADD)
	public Result addLanguage(@QueryParam("locale") String locale) {
		return portalService.addLanguage(locale);
	}

	@Override
	@GET
	@Path(LANGUAGE + REMOVE)
	public Result removeLanguage(@QueryParam("locale") String locale) {
		return portalService.removeLanguage(locale);
	}

	/* WS for citizen */
	@Override
	@POST
	@Path(USER + PROFILE)
	public UserProfile getCitizenProfile(Filter filter) {
		return userService.getCitizenProfile(filter);
	}

	@Override
	@POST
	@Path(USER + PROFILE + UPDATE)
	public Result setProfile(UserProfile profile) {
		return userService.setProfile(profile);
	}

	@Override
	@POST
	@Path(USER + PROFILE + PICTURE + UPDATE)
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	public Result setProfilePicture(
			@FormDataParam("file") InputStream stream,
			@FormDataParam("file") FormDataContentDisposition info,
			@PathParam("personalId") String personalId
	) {
		return userService.setProfilePicture(stream, info, personalId);
	}

	@Override
	@GET
	@Path(PortalGateway.LOGOUT)
	public String logout() {
		return portalService.logout();
	}

	@Override
	@GET
	@Path(PortalGateway.REMIND_PASSWORD)
	public String doRemindPassword(@QueryParam("ssn") String ssn) {
		return portalService.doRemindPassword(ssn);
	}

	@Override
	@GET
	@Path(PortalGateway.PING)
	public Result doPing() {
		return portalService.doPing();
	}

}