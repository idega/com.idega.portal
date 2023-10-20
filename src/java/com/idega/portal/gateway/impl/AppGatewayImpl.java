package com.idega.portal.gateway.impl;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.idega.block.oauth2.server.authentication.bean.AccessToken;
import com.idega.portal.PortalConstants;
import com.idega.portal.business.PortalUserManager;
import com.idega.portal.gateway.AppGateway;
import com.idega.portal.gateway.PortalGateway;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.model.UserProfile;
import com.idega.portal.service.AppService;
import com.idega.restful.business.DefaultRestfulService;

@Component
@Path(AppGateway.PATH)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class AppGatewayImpl extends DefaultRestfulService implements AppGateway {

	@Autowired
	private PortalUserManager portalUserManager;

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_APP)
	private AppService appService;

	@Override
	@GET
	@Path(PortalGateway.SETTINGS)
	public PortalSettings getDashboardSettings(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return appService.getDashboardSettings(
				request,
				response,
				context
		);
	}

	@Override
	@GET
	@Path(AppGateway.ACCESS_TOKEN)
	public AccessToken getAccessToken(
			@QueryParam("uuid") String uuid,
			@QueryParam("token") String token,
			@QueryParam("clientId") String clientId,
			@QueryParam("type") String type,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalUserManager.getAccessToken(uuid, token, clientId, type, request, response, context);
	}

	@Override
	@GET
	@Path(PortalGateway.LOGOUT)
	public String logout(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return appService.logout(request, response, context);
	}

	@Override
	@POST
	@Path(AppGateway.CREDENTIALS)
	public Result isValidLogin(
			UserAccount credentials,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return appService.isValidLogin(credentials, request, response, context);
	}

	@Override
	@GET
	@Path(PortalGateway.USER)
	public UserProfile getCitizenProfile(
			@QueryParam("personalId") String personalId,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return appService.getCitizenProfile(personalId, request, response, context);
	}

}