package com.idega.portal.gateway.impl;

import java.io.IOException;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.idega.portal.PortalConstants;
import com.idega.portal.gateway.PortalGateway;
import com.idega.portal.model.Article;
import com.idega.portal.model.ArticleList;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.LocalizedArticle;
import com.idega.portal.model.LocalizedArticleList;
import com.idega.portal.model.LoginResult;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.service.PortalService;
import com.idega.restful.business.DefaultRestfulService;
import com.idega.restful.exception.Unauthorized;

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
	public PortalSettings getDashboardSettings(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.getDashboardSettings(
				request,
				response,
				context
		);
	}

	@Override
	@POST
	@Path(PortalGateway.ACCOUNT)
	public UserAccount doCreateAccount(
			UserAccount account,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.doCreateAccount(
				account,
				request,
				response,
				context
		);
	}

	@Override
	@GET
	@Path(PortalGateway.MENUS)
	public List<PortalMenu> getPortalMenus(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;	//	User must be authorized for this WS
	}

	@Override
	@GET
	@Path(PortalGateway.LANGUAGE)
	public String setLanguage(
			@QueryParam("language") String language,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.setLanguage(
				language,
				request,
				response,
				context
		);
	}

	@Override
	@POST
	@Path(PortalGateway.LANGUAGE + PortalGateway.LOCALIZE)
	public Result setLocalization(
			Localization localization,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
	}

	@Override
	@POST
	@Path(PortalGateway.LANGUAGE + PortalGateway.LOCALIZE + PortalGateway.STRINGS)
	public Result setLocalizations(
			Localizations localizations,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
	}

	@Override
	@POST
	@Path(PortalGateway.LOCALIZE_ARTICLES)
	public void localizeArticles(
			LocalizedArticleList localizedArticlesMap,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) throws IOException {
		throw new Unauthorized();
	}

	@Override
	@GET
	@Path(PortalGateway.LOCALIZED_ARTICLE)
	public List<LocalizedArticle> getLocalizedArticles(
			@QueryParam("uri")List<String> uris,
			@QueryParam("locale")List<String> locales,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response, @Context ServletContext context
	) throws IOException {
		return portalService.getLocalizedArticles(
				uris,
				locales,
				request,
				response, context
		);
	}

	@Override
	@GET
	@Path(PortalGateway.LANGUAGE + PortalGateway.AVAILABLE)
	public List<LanguageData> getAvailableLanguages(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
	}

	@Override
	@GET
	@Path(PortalGateway.LANGUAGE + PortalGateway.ADD)
	public Result addLanguage(
			@QueryParam("locale") String locale,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
	}

	@Override
	@GET
	@Path(PortalGateway.LANGUAGE + PortalGateway.REMOVE)
	public Result removeLanguage(
			@QueryParam("locale") String locale,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
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
	@Path(PortalGateway.UNAUTHORIZE)
	public String doUnAuthorizeViaGateway(
			@Context HttpServletRequest httpRequest,
			@Context HttpServletResponse httpResponse,
			@QueryParam("uri") String uri
	) {
		return portalService.doUnAuthorizeViaGateway(httpRequest, httpResponse, uri);
	}

	@Override
	@GET
	@Path(PortalGateway.LOGOUT)
	public String logout(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.logout(
				request,
				response,
				context
		);
	}

	@Override
	@GET
	@Path(PortalGateway.REMIND_PASSWORD)
	public String doRemindPassword(
			@QueryParam("ssn") String ssn,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.doRemindPassword(
				ssn,
				request,
				response,
				context
		);
	}

	@Override
	@GET
	@Path(PortalGateway.PING)
	public Result doPing(
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return null;
	}

	@GET
	@Path(PortalGateway.ARTICLE)
	@Override
	public Article getArticleByURI(
			@QueryParam("uri") String uri,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.getArticleByURI(
				uri,
				request,
				response,
				context
		);
	}

	@GET
	@Path(PortalGateway.ARTICLE_LOCALIZED)
	@Override
	public Article getLocalizedArticle(
			@QueryParam("url") String url,
			@PathParam("language") String language,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) throws IOException {
		return portalService.getLocalizedArticle(
				url,
				language,
				request,
				response,
				context
		);
	}

	@GET
	@Path(PortalGateway.ARTICLES)
	@Override
	public ArticleList getArticlesByCategory(
			@QueryParam("category") String category,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.getArticlesByCategory(
				category,
				request,
				response,
				context
		);
	}

	@POST
	@Path(PortalGateway.LOGIN)
	@Override
	@Consumes(MediaType.APPLICATION_FORM_URLENCODED)
	public LoginResult login(
			@FormParam("client_id") String clientId,
			@FormParam("username") String username,
			@FormParam("password") String password,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.login(clientId, username, password, request, response, context);
	}

	@POST
	@Path(PortalGateway.PASSWORD_TOKEN_PASSWORD)
	@Override
	public String doUpdatePassword(
			@PathParam("tokenId")String token,
			String newPassword,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.doUpdatePassword(
				token,
				newPassword,
				request,
				response,
				context
		);
	}

	@GET
	@Path(PortalGateway.PASSWORD_TOKEN_BY_ID)
	@Override
	public String isUpdatePasswordLinkValid(
			@PathParam("tokenId")String token,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.isUpdatePasswordLinkValid(
				token,
				request,
				response,
				context
		);
	}

	@Override
	@GET
	@Path(PortalGateway.FILE)
	public Response getRepositoryFile(
			@PathParam("identifier") String identifier,
			@Context HttpServletRequest request,
			@Context HttpServletResponse response,
			@Context ServletContext context
	) {
		return portalService.getRepositoryFile(identifier, request, response, context);
	}

}