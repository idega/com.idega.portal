package com.idega.portal.gateway.impl;

import java.util.List;

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

import com.idega.portal.PortalConstants;
import com.idega.portal.gateway.PortalGateway;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.UserAccount;
import com.idega.portal.service.PortalService;
import com.idega.restful.business.DefaultRestfulService;

@Component
@Path(PortalGateway.PORTAL)
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class UnAuthorizedPortalGatewayImpl extends DefaultRestfulService implements PortalService {

	@Autowired
	@Qualifier(PortalConstants.QUALIFIER_PORTAL)
	private PortalService portalService;

	@Override
	@GET
	@Path(PortalGateway.SETTINGS)
	public PortalSettings getDashboardSettings() {
		return portalService.getDashboardSettings();
	}

	@Override
	@POST
	@Path(PortalGateway.ACCOUNT)
	public UserAccount doCreateAccount(UserAccount account) {
		return portalService.doCreateAccount(account);
	}

	@Override
	@GET
	@Path(PortalGateway.MENUS)
	public List<PortalMenu> getPortalMenus() {
		return null;	//	User must be authorized for this WS
	}

	@Override
	@GET
	@Path(PortalGateway.LANGUAGE)
	public String setLanguage(@QueryParam("language") String language) {
		return portalService.setLanguage(language);
	}

	@Override
	@GET
	@Path(PortalGateway.AUTHORIZE)
	public String doAuthorizeViaGateway(
			@Context HttpServletRequest httpRequest,
			@Context HttpServletResponse httpResponse,
			@QueryParam("type") String type
	) {
		return portalService.doAuthorizeViaGateway(httpRequest, httpResponse, type);
	}

	@Override
	@GET
	@Path(PortalGateway.LOGOUT)
	public String logout() {
		return portalService.logout();
	}

}