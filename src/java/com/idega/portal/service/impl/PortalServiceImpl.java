package com.idega.portal.service.impl;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.logging.Level;

import javax.annotation.PostConstruct;
import javax.ejb.FinderException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.StreamingOutput;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.article.bean.ArticleItemBean;
import com.idega.block.article.data.ArticleEntity;
import com.idega.block.article.data.dao.ArticleDao;
import com.idega.block.login.bean.OAuthToken;
import com.idega.block.login.business.OAuth2Service;
import com.idega.block.login.business.PasswordTokenBusiness;
import com.idega.block.login.data.PasswordTokenEntity;
import com.idega.block.login.data.dao.PasswordTokenEntityDAO;
import com.idega.block.login.presentation.Login;
import com.idega.content.upload.business.UploadAreaBean;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.accesscontrol.business.LoginBusinessBean;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.business.LoginState;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.accesscontrol.data.LoginTableHome;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.data.Email;
import com.idega.core.file.data.ICFile;
import com.idega.core.file.data.ICFileHome;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.core.location.data.Address;
import com.idega.core.location.data.AddressHome;
import com.idega.data.IDOLookup;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.jackrabbit.security.RepositoryAccessManager;
import com.idega.portal.PortalConstants;
import com.idega.portal.business.AccountCreatedMessageSender;
import com.idega.portal.business.DefaultAccountCreatedMessageSender;
import com.idega.portal.business.MessageSender;
import com.idega.portal.model.Article;
import com.idega.portal.model.ArticleList;
import com.idega.portal.model.FileUploadResult;
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
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.LocalizationService;
import com.idega.portal.service.PortalService;
import com.idega.portal.service.PortalSettingsResolver;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.repository.jcr.JCRItem;
import com.idega.restful.exception.BadRequest;
import com.idega.restful.exception.Forbidden;
import com.idega.restful.exception.InternalServerError;
import com.idega.restful.exception.NotFound;
import com.idega.restful.exception.Unauthorized;
import com.idega.user.bean.UserDataBean;
import com.idega.user.business.CompanyHelper;
import com.idega.user.business.GroupBusiness;
import com.idega.user.business.StandardGroup;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.Group;
import com.idega.user.data.MetadataConstants;
import com.idega.user.data.bean.User;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.WebUtil;
import com.idega.util.datastructures.map.MapUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.text.Name;
import com.sun.jersey.core.header.ContentDisposition;
import com.sun.jersey.multipart.FormDataBodyPart;
import com.sun.jersey.multipart.FormDataMultiPart;

@Service
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Qualifier(PortalConstants.QUALIFIER_PORTAL)
public class PortalServiceImpl extends DefaultSpringBean implements PortalService {

	@Autowired
	private WebUtil webUtil;

	@Autowired
	private LocalizationService localizationService;

	@Autowired
	private ArticleDao articleDAO;

	@Autowired
	private OAuth2Service oauth2Service;

	@Qualifier("citizenStandardGroup")
	@Autowired(required = false)
	private StandardGroup standardGroup;

	@Autowired
	private PasswordTokenEntityDAO passwordTokenEntityDAO;

	@Autowired
	private PasswordTokenBusiness passwordTokenBusiness;

	@Autowired(required = false)
	private List<AccountCreatedMessageSender> customAccountCreatedMessagesSenders;

	@Autowired(required = false)
	private CompanyHelper companyHelper;

	@Autowired
	private RepositoryAccessManager repositoryAccessManager;

	private List<? extends MessageSender> accountCreatedMessagesSenders;

	@PostConstruct
    private void initAccountCreatedMessagesSenders() {
        if(ListUtil.isEmpty(customAccountCreatedMessagesSenders)) {
        	accountCreatedMessagesSenders = Arrays.asList(
        			new DefaultAccountCreatedMessageSender()
        	);
        	return;
        }
        accountCreatedMessagesSenders = customAccountCreatedMessagesSenders;
    }

	private StandardGroup getStandardGroup() {
		if (standardGroup == null) {
			try {
				ELUtil.getInstance().autowire(this);
			} catch (Exception e) {}
		}
		return standardGroup;
	}

	private OAuth2Service getOAuth2Service() {
		if (oauth2Service == null) {
			ELUtil.getInstance().autowire(this);
		}
		return oauth2Service;
	}

	@Override
	public PortalSettings getDashboardSettings(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return new PortalSettingsResolver().getDashboardSettings(new IWContext(request, response, context));
	}

	private UserBusiness getUserBusiness() {
		return getServiceInstance(UserBusiness.class);
	}

	@Override
	public List<PortalMenu> getPortalMenus(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		User user = null;
		try {
			user = SecurityUtil.getInstance().getAuthorizedUser(new IWContext(request, response, context));
			return getMenus(user);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting menu for " + user, e);
		}
		return null;
	}

	private List<PortalMenu> getMenus(User user) {
		List<PortalMenu> menus = new ArrayList<>();
		if (user == null) {
			getLogger().warning("User is unknown");
			return menus;
		}

		return menus;
	}

	@Override
	public UserAccount doCreateAccount(UserAccount account, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER));

		if (account == null) {
			account = new UserAccount();
			account.setErrorMessage(iwrb.getLocalizedString("account.account_data_not_provided", "Please provide data for account"));
			return account;
		}

		if (StringUtil.isEmpty(account.getUsername()) || StringUtil.isEmpty(account.getPassword())) {
			account.setErrorMessage(iwrb.getLocalizedString("account.provide_credentials", "Please provide credentials for account"));
			return account;
		}

		try {
			//Fix the personal ids
			if (account != null) {
				if (!StringUtil.isEmpty(account.getPersonalId())) {
					account.setPersonalId(account.getPersonalId().replaceAll(CoreConstants.MINUS, CoreConstants.EMPTY));
				}
				if (!StringUtil.isEmpty(account.getPersonalIdOfResponsiblePerson())) {
					account.setPersonalIdOfResponsiblePerson(account.getPersonalIdOfResponsiblePerson().replaceAll(CoreConstants.MINUS, CoreConstants.EMPTY));
				}
			}

			IWContext iwc = new IWContext(request, response, context);
			UserBusiness userBusiness = getUserBusiness();

			String uuid = account.getUuid();
			LoginTable login = LoginDBHandler.getUserLoginByUserName(account.getUsername());
			if (login != null) {
				boolean error = true;

				if (!StringUtil.isEmpty(uuid)) {
					int userId = login.getUserId();
					com.idega.user.data.User providedUser = null;
					try {
						providedUser = userBusiness.getUserByUniqueId(uuid);
					} catch (Exception e) {}
					if (providedUser != null && Integer.valueOf(providedUser.getId()).intValue() == userId) {
						error = false;
						getLogger().info(
								"Found login (" + login + ") with username '" + account.getUsername() + "' for user " + login.getUser() +
								" (ID: " + userId + "). Login can be used because it belongs to the same person " +
								(providedUser == null ? CoreConstants.EMPTY : (providedUser + " (ID: " + providedUser.getId() + ")"))
						);
					} else {
						getLogger().warning(
								"Found login (" + login + ") with username '" + account.getUsername() + "' for user " + login.getUser() +
								" (ID: " + userId + "). Login can not be used by another user " +
								(providedUser == null ? CoreConstants.EMPTY : (providedUser + " (ID: " + providedUser.getId() + ")"))
						);
					}
				}

				if (error) {
					account.setErrorMessage(iwrb.getLocalizedString("account.please_choose_different_user_name", "Please choose different username"));
					return account;
				}
			}

			String serverName = request.getServerName();
			boolean validateCompanyId = getSettings().getBoolean("portal.validate_com_id_" + serverName, true);
			boolean validatePersonalId = getSettings().getBoolean("portal.validate_p_id_" + serverName, true);
			boolean personalIdProvided = true;

			String personalId = account.getPersonalId();
			if (StringUtil.isEmpty(personalId)) {
				personalIdProvided = false;
				IWTimestamp now = IWTimestamp.RightNow();
				String date = now.getDateString("ddMMYY");
				String id = String.format("%04d", new Random().nextInt(10000));
				personalId = date.concat(id);
				account.setPersonalId(personalId);

				if (account.isPersonAsCompany()) {
					validateCompanyId = false;
				} else {
					validatePersonalId = false;
				}
			}

			UserDataBean company = null;
			try {
				company = companyHelper == null ? null : companyHelper.getCompanyInfo(account.getPersonalId());
			} catch (Exception e) {}

			if (account.isPersonAsCompany() && company == null) {
				//	Creating company with citizen's personal ID
				company = companyHelper == null ? null : companyHelper.doCreateCompany(account.getName(), account.getPersonalId());
			}

			com.idega.user.data.User user = null;

			String firstName = null;
			String middleName = null;
			String lastName = null;
			String displayName = null;
			String name = StringUtil.isEmpty(account.getNameOfResponsiblePerson()) ? account.getName() : account.getNameOfResponsiblePerson();
			Name userNames = null;
			Integer genderId = null;

			com.idega.user.data.User tmpUser = null;
			if (company == null) {
				try {
					tmpUser = userBusiness.getCorrectUserCheckedByEmail(account.getPersonalId(), account.getUsername());
					getLogger().info("tmpUser: " + tmpUser);
				} catch (Exception e) {}
			}
			if (tmpUser == null && !StringUtil.isEmpty(account.getPersonalIdOfResponsiblePerson())) {
				try {
					tmpUser = userBusiness.getUser(account.getPersonalIdOfResponsiblePerson());
				} catch (Exception e) {}
			}
			if (tmpUser == null) {
				if (company == null) {
					userNames = StringUtil.isEmpty(name) ? null : new Name(name);
					displayName = name;
				} else {
					name = company.getName();
					displayName = name;
					firstName = name;
				}
			} else {
				name = tmpUser.getName();
				userNames = new Name(name);
				displayName = name;

				int tmpGenderId = tmpUser.getGenderID();
				if (tmpGenderId > 0) {
					genderId = tmpGenderId;
				}
			}
			if (userNames != null) {
				firstName = userNames.getFirstName();
				middleName = userNames.getMiddleName();
				lastName = userNames.getLastName();
			}

			if (company == null) {
				if (!validateCompanyId) {
					company = companyHelper == null ? null : companyHelper.doCreateCompany(account.getName(), account.getPersonalId());
				}
			}
			if (company == null) {
				boolean error = false;
				if (validatePersonalId) {
					error = !userBusiness.validatePersonalId(account.getPersonalId(), getCurrentLocale());
				}
				if (error) {
					getLogger().warning("Provided invalid personal ID: " + account.getPersonalId());
					account.setErrorMessage(iwrb.getLocalizedString("account.provide_valid_personal_id", "Please provide valid personal ID"));
					return account;
				}
			}

			Group companyGroup = null;
			GroupBusiness groupBusiness = getServiceInstance(GroupBusiness.class);
			if (company != null && company.getGroupId() != null) {
				companyGroup = groupBusiness.getGroupByGroupID(company.getGroupId());
			}

			String personalIdForUser = StringUtil.isEmpty(account.getPersonalIdOfResponsiblePerson()) ? account.getPersonalId() : account.getPersonalIdOfResponsiblePerson();
			try {
				user = userBusiness.getCorrectUserCheckedByEmail(personalIdForUser, account.getEmail());
				getLogger().info("user by getCorrectUserCheckedByEmail. With params: personalIdForUser = " + personalIdForUser + ", email = " + account.getEmail() +
						". User: " + user);
			} catch (Exception e) {}

			boolean newLogin = StringUtil.isEmpty(uuid);
			if (newLogin) {
				StandardGroup standardGroup = null;
				if (companyGroup == null) {
					try {
						standardGroup = getStandardGroup();
					} catch (Throwable t) {}
				}
				getLogger().info("Creating a new login (username: '" + account.getEmail()+ "') for: " + personalIdForUser);
				user = userBusiness.createUserWithLogin(
						firstName,
						middleName,
						lastName,
						personalIdForUser,
						displayName,
						null,						//	Description
						genderId,					//	Gender
						null,						//	Date of birth
						companyGroup == null ?
								standardGroup == null ? null : Integer.valueOf(standardGroup.getGroup().getPrimaryKey().toString()) :
								(Integer) companyGroup.getPrimaryKey(),
						account.getUsername(),
						account.getPassword(),
						Boolean.TRUE,
						IWTimestamp.RightNow(),
						5000,
						Boolean.FALSE,
						Boolean.TRUE,
						Boolean.FALSE,
						null
				);
			} else if (account.isChangePassword()) {
				getLogger().info("Update login for: " + user + " (ID: " + (user == null ? "unknown" : user.getId()) + ")");
				user = userBusiness.update(
						user == null ? null : user.getId(),
						uuid,
						null,
						firstName,
						middleName,
						lastName,
						displayName,
						personalIdForUser,
						account.getEmail(),
						null,
						account.getUsername(),
						account.getPassword()
				);
			}

			if (user == null) {
				account.setErrorMessage(iwrb.getLocalizedString("account.failed_to_create_account", "Sorry, some error occurred - failed to create account. Please try later"));
				return account;
			}

			if (companyGroup != null) {
				Integer groupId = company.getGroupId();
				groupBusiness.addUser(groupId, user);

				user.setMetaData(MetadataConstants.USER_REAL_COMPANY_META_DATA_KEY, company.getGroupId().toString());
				user.store();
				CoreUtil.clearIDOCaches();

				String addressId = company.getAddressId();
				if (StringHandler.isNumeric(addressId)) {
					try {
						AddressHome addressHome = (AddressHome) IDOLookup.getHome(Address.class);
						Address address = addressHome.findByPrimaryKey(Integer.valueOf(addressId));
						user.addAddress(address);
						user.store();
					} catch (Exception e) {
						getLogger().log(Level.WARNING, "Error updating user's address by company's address ID: " + addressId, e);
					}
				} else {
					getLogger().warning("Unknown address for company " + user);
				}

				String defaultRolesForCompanyProp = getSettings().getProperty("dashboard.default_roles_for_company");
				if (!StringUtil.isEmpty(defaultRolesForCompanyProp)) {
					String[] defaultRolesForCompany = defaultRolesForCompanyProp.split(CoreConstants.COMMA);
					if (!ArrayUtil.isEmpty(defaultRolesForCompany)) {
						AccessController accessController = iwc.getAccessController();
						for (String role: defaultRolesForCompany) {
							if (StringUtil.isEmpty(role)) {
								continue;
							}

							accessController.addRoleToGroup(role, groupId, iwc);
						}
					}
				}
			}

			if (!StringUtil.isEmpty(account.getEmail())) {
				Email email = userBusiness.updateUserMail(user, account.getEmail());
				if (email == null) {
					getLogger().log(Level.WARNING, "Error updating email: " + account.getEmail() + " for user (ID: " + user.getPrimaryKey().toString() + ")");
				}
				if (account.isSendEmail()) {
					sendAccountCreatedMessage(user, iwc.getLocale(), personalIdProvided, account.getEmail());
				}
			}
			account.setUserId(user.getPrimaryKey().toString());
			account.setErrorMessage(null);
		} catch (Exception e) {
			account.setErrorMessage(iwrb.getLocalizedString("account.failed_to_create_account", "Sorry, some error occurred - failed to create account. Please try later"));
			getLogger().log(Level.WARNING, "Error creating account for " + account.getUsername(), e);
		}

		return account;
	}

	private void sendAccountCreatedMessage(
			com.idega.user.data.User user,
			Locale locale,
			boolean personalIdAsUserName,
			String emailAddress
	) throws Exception {
		for (MessageSender sender: accountCreatedMessagesSenders) {
			sender.sendUserMessages(user, locale, personalIdAsUserName, emailAddress);
		}
	}

	@Override
	public String doAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String type) {
		getLogger().warning("This method is not implemented");
		return null;
	}

	@Override
	public String doUnAuthorizeViaGateway(HttpServletRequest httpRequest, HttpServletResponse httpResponse, String uri) {
		getLogger().warning("This method is not implemented");
		return null;
	}

	@Override
	public String setLanguage(String language, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.setLanguage(language, request, response, context);
	}

	@Override
	public String logout(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return webUtil.logOut();
	}

	@Override
	@Transactional
	public String doRemindPassword(String ssn, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		if (StringUtil.isEmpty(ssn)) {
			throw new BadRequest("User '"+ssn+"' not found");
		}

		IWContext iwc = new IWContext(request, response, context);
		IWResourceBundle iwrb = getResourceBundle(getBundle(PortalConstants.IW_BUNDLE_IDENTIFIER), iwc);
		UserBusiness userBusiness = getUserBusiness();

		com.idega.user.data.User user = null;
		try {
			user = userBusiness.getUser(ssn);
		} catch (Exception e) {}
		if (user == null) {
			LoginTable loginTable = null;
			try {
				loginTable = LoginDBHandler.getUserLoginByUserName(ssn);
			} catch (Exception e) {}
			if (loginTable != null && loginTable.getUserId() > 0) {
				try {
					user = userBusiness.getUser(loginTable.getUserId());
				} catch (Exception e) {}
			}
		}
		if (user == null) {
			throw new BadRequest("User '" + ssn + "' not found");
		}

		Email email = null;
		try {
			email = user.getUsersEmail();
		} catch (Exception e) {}
		if (email == null) {
			Collection<Email> emails = user.getEmails();
			email = ListUtil.isEmpty(emails) ? null : emails.iterator().next();
		}
		String emailAddress = email == null ? null : email.getEmailAddress();
		if (StringUtil.isEmpty(emailAddress)) {
			throw new BadRequest("User '" + ssn + "' does not have email address");
		}
		PasswordTokenEntity passwordToken = passwordTokenEntityDAO.create(
				user.getUniqueId(),
				iwc.getRemoteIpAddress(),
				Long.valueOf(86400000) // 24 hours
		);
		sendPasswordResetLink(user, iwc, emailAddress, passwordToken, iwrb);
		return emailAddress;
	}

	private void sendPasswordResetLink(
			com.idega.user.data.User user,
			IWContext iwc,
			String emailTo,
			PasswordTokenEntity passwordToken,
			IWResourceBundle iwrb
	) {
		String link = passwordTokenBusiness.getLink(passwordToken, iwc);
		String systemName = iwrb.getLocalizedString("message.email.password_remind.body.system_name", "Idega");
		String message = iwrb.getLocalizedAndFormattedString(
				"message.email.password_remind.body",
				"Hi {0}.<br /><br />A new password for {1} has been requested in the {2} database. To set a new password, it is necessary to open the following URL " +
				"(the URL must be inserted as one continuous line in the browser):<br /><br />{3}<br /><br />This URL is active for 1 day after the request was received, after " +
				"having to repeat the password change request.",
				new Object[] {
					user.getDisplayName(),
					user.getFirstName(),
					systemName,
					link
				}
		);

		//With regards text
		String key = "message.email.account_created.body.with_regards";
		String regards = iwrb.getLocalizedString(key, key);
		if (StringUtil.isEmpty(regards) || regards.equals(key)) {
			regards = getSettings().getProperty("with_regards_text");
		}
		if (!StringUtil.isEmpty(regards) && !regards.equals(key)) {
			message += "<br /><br />" + regards;
		}

		passwordTokenBusiness.notifyRegisteredUser(
				passwordToken,
				iwc,
				message.toString()
		);
	}

	@Override
	public Result setLocalization(Localization localization, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.setLocalization(localization, request, response, context);
	}

	@Override
	public Result setLocalizations(Localizations localizations, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.setLocalizations(localizations, request, response, context);
	}

	@Override
	public List<LanguageData> getAvailableLanguages(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.getAvailableLanguages(request, response, context);
	}

	@Override
	public Result addLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.addLanguage(locale, request, response, context);
	}

	@Override
	public Result removeLanguage(String locale, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return localizationService.removeLanguage(locale, request, response, context);
	}

	@Override
	public void localizeArticles(
			LocalizedArticleList localizedArticlesList,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) throws IOException {
		IWContext iwc = new IWContext(request, response, context);
		User user = SecurityUtil.getInstance().getAuthorizedUser(iwc);
		if (user == null) {
			throw new Unauthorized();
		}
		if (!SecurityUtil.getInstance().hasAnyRole(
				iwc,
				user,
				Arrays.asList("dashboard.admin")
		)) {
			throw new Forbidden();
		}
		List<LocalizedArticle> articles = localizedArticlesList.getArticles();
		if (ListUtil.isEmpty(articles)) {
			return;
		}
		List<ArticleItemBean> articlesToSave = new ArrayList<>();
		for(LocalizedArticle article: articles) {
			String localeString = article.getLocale();
			String language = localeString.substring(0, 2);
			ArticleItemBean bean = new ArticleItemBean();
			bean.setResourcePath(article.getUrl());
			bean.setLanguage(language);
			bean.load();
			bean.setBody(article.getBody());
			bean.setHeadline(article.getTitle());
			articlesToSave.add(bean);
		}

		// Saving after everything to had better chance of transaction like behavior
		for (ArticleItemBean bean: articlesToSave) {
			bean.store();
		}
	}

	@Override
	public List<LocalizedArticle> getLocalizedArticles(
			List<String> uris,
			List<String> locales,
			HttpServletRequest request,
			HttpServletResponse response, ServletContext context
	) throws IOException {
		List<LocalizedArticle> articles = new ArrayList<>();
		Map<String,String> languages = new HashMap<>(locales.size());
		for (String locale: locales) {
			String language = locale.substring(0, 2);
			languages.put(language, locale);
		}
		for(String resourcePath: uris) {
			for (Map.Entry<String, String> entry : languages.entrySet()) {
				String language = entry.getKey();
				String locale = entry.getValue();
				ArticleItemBean bean = new ArticleItemBean();
				bean.setResourcePath(resourcePath);
				bean.setLanguage(language);
				bean.load();
				if(!bean.getExists()) {
					continue;
				}
				LocalizedArticle article = new LocalizedArticle();
				article.setTitle(bean.getHeadline());
				String body = bean.getBody();
				article.setBody(body);
				article.setBodyRaw(body);
				article.setUrl(resourcePath);
				article.setLocale(locale);
				articles.add(article);
			}
		}
		return articles;
	}

	@Override
	public Result doPing(HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		try {
			return new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
		} catch (Exception e) {}
		return new Result(Status.INTERNAL_SERVER_ERROR.getStatusCode(), Boolean.FALSE.toString());
	}

	@Override
	public Article getArticleByURI(String uri, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		Article article = null;
		try {
			if (StringUtil.isEmpty(uri)) {
				return article;
			}

			if (StringUtil.isEmpty(uri) || (uri.equals(CoreConstants.SLASH))) {
				getLogger().warning("URI is invalid: " + uri);
				return null;
			}

			ArticleItemBean articleItemBean = new ArticleItemBean();
			articleItemBean.setResourcePath(uri);
			articleItemBean.load();

			article = new Article();
			article.setTitle(articleItemBean.getHeadline());
			String body = articleItemBean.getBody();
			article.setBody(body);
			article.setBodyRaw(body);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error loading article at " + uri, e);
		}
		return article;
	}

	@Override
	public Article getLocalizedArticle(
			String url,
			String language,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) throws IOException {
		ArticleItemBean bean = new ArticleItemBean();
		bean.setResourcePath(url);
		bean.setLanguage(language);
		bean.load();
		if(!bean.getExists()) {
			throw new NotFound(
					"Article ("
							+ url
							+ ") not found for language ("
							+ language
							+ ")"
			);
		}
		Article article = new Article();
		article.setTitle(bean.getHeadline());
		String body = bean.getBody();
		article.setBody(body);
		article.setBodyRaw(body);
		return article;
	}

	@Override
	public ArticleList getArticlesByCategory(String category, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		if (category == null) {
			return null;
		}

		List<String> categories = new ArrayList<>();
		categories.add(category);

		List<ArticleEntity> articles = articleDAO.getByCategories(categories, null, 0);
		if (ListUtil.isEmpty(articles)) {
			return null;
		}

		Collections.sort(articles, new Comparator<ArticleEntity>() {

			@Override
			public int compare(ArticleEntity a1, ArticleEntity a2) {
				Date date1 = a1.getModificationDate();
				Date date2 = a2.getModificationDate();
				if (date1 == null || date2 == null) {
					return 0;
				}

				return date1.compareTo(date2);
			}

		});

		List<ArticleItemBean> articleBeans = new ArrayList<>();
		for (ArticleEntity article: articles) {
			try {
				String uri = article.getUri();
				if (StringUtil.isEmpty(uri) || (uri.equals(CoreConstants.SLASH))) {
					getLogger().warning("URI is invalid: " + uri);
					continue;
				}

				ArticleItemBean articleItemBean = new ArticleItemBean();
				articleItemBean.setResourcePath(uri);
				articleItemBean.load();

				articleBeans.add(articleItemBean);
			} catch (Exception e) {
				getLogger().warning("Error loading article at " + article.getUri());
			}
		}

		ArticleList result = new ArticleList();
		List<Article> art = new ArrayList<>();
		result.setArticles(art);
		for (ArticleItemBean article: articleBeans) {
			art.add(new Article(article));
		}
		return result;
	}

	@Override
	public LoginResult login(String clientId, String username, String password, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		String error = null;
		if (request == null) {
			error = "Request is unknown";
			getLogger().warning(error);
			return new LoginResult(-1, error, "login_error.request_unknown");
		}

		IWResourceBundle iwrb = null;
		try {
			IWContext iwc = new IWContext(request, response, context);
			iwrb = getResourceBundle(getApplication().getBundle(Login.IW_BUNDLE_IDENTIFIER));

			LoginBusinessBean loginBusiness = LoginBusinessBean.getLoginBusinessBean(iwc);

			//	Logout
			if (iwc.isLoggedOn()) {
				try {
					loginBusiness.logOutUser(iwc);
				} catch (Exception e) {}
			}

			//	Check username
			username = StringUtil.isEmpty(username) ? request.getHeader("username") : username;
			if (StringUtil.isEmpty(username) || CoreConstants.UNDEFINED.equals(username)) {
				error = "Username is not provided";
				getLogger().warning(error);
				return new LoginResult(-1, error, "login_error.username_is_not_provided");
			}

			//	Check password
			password = StringUtil.isEmpty(password) ? request.getHeader("password") : password;
			if (StringUtil.isEmpty(password) || CoreConstants.UNDEFINED.equals(password)) {
				error = "Password is not provided";
				getLogger().warning(error + ". Username: " + username);
				return new LoginResult(-1, error, "login_error.password_is_not_provided");
			}

			String redirections = getSettings().getProperty("dashboard.redirections");
			if (!StringUtil.isEmpty(redirections)) {
				String[] usernamesWithRedirections = redirections.split(CoreConstants.COMMA);
				if (!ArrayUtil.isEmpty(usernamesWithRedirections)) {
					for (String userNameWithRedirection: usernamesWithRedirections) {
						if (StringUtil.isEmpty(userNameWithRedirection)) {
							continue;
						}

						String[] usernameAndRedirection = userNameWithRedirection.split(CoreConstants.EQ);
						if (!ArrayUtil.isEmpty(usernameAndRedirection) && usernameAndRedirection.length == 2 && usernameAndRedirection[0].equals(username)) {
							LoginResult result = new LoginResult();
							result.setRedirect(usernameAndRedirection[1]);
							return result;
						}
					}
				}
			}

			if (!loginBusiness.logInUser(request, username, password)) {
				LoginState state = LoginBusinessBean.internalGetState(iwc);
				String errorLocalizedKey = "login_failed";
				error = iwrb.getLocalizedString(errorLocalizedKey, "Login failed");
				if (state != null) {
					if (state.equals(LoginState.FAILED)) {
						errorLocalizedKey = "login_failed";
						error = iwrb.getLocalizedString(errorLocalizedKey, "Login failed");

					} else if (state.equals(LoginState.USER_NOT_FOUND)) {
						errorLocalizedKey = "login_no_user";
						error = iwrb.getLocalizedString(errorLocalizedKey, "Invalid user");

					} else if (state.equals(LoginState.WRONG_PASSWORD)) {
						errorLocalizedKey = "login_wrong";
						error = iwrb.getLocalizedString(errorLocalizedKey, "Invalid password");

					} else if (state.equals(LoginState.EXPIRED)) {
						errorLocalizedKey = "login_expired";
						error = iwrb.getLocalizedString(errorLocalizedKey, "Login expired");

					} else if (state.equals(LoginState.FAILED_DISABLED_NEXT_TIME)){
						errorLocalizedKey = "login_wrong_disabled_next_time";
						error = iwrb.getLocalizedString(errorLocalizedKey, "Invalid password, access closed next time login fails");
					}
				}
				getLogger().warning((error == null ? "Login failed" : error) + ". State: " + state + ", username " + username + " and password: " + password);
				return new LoginResult(-1, error, errorLocalizedKey);
			}

			clientId = StringUtil.isEmpty(clientId) ? request.getHeader("client_id") : clientId;
			if (StringUtil.isEmpty(clientId) || CoreConstants.UNDEFINED.equals(clientId)) {
				clientId = request.getParameter("client_id");
			}
			OAuthToken token = getOAuth2Service().getToken(request, clientId, username, password);
			if (token == null) {
				error = "Failed to authorize, please try again a bit later";
				getLogger().warning(error + ": did not get OAuth token for username " + username + " and password: " + password);
				return new LoginResult(-1, error, "login_error.failed_to_get_oauth_token");
			} else if (StringUtil.isEmpty(token.getScope())) {
				token.setScope("read write trust");
			}

			LoginResult success = new LoginResult(token);
			return success;
		} catch (Exception e) {
			String errorLocalizedKey = "login_failed";
			error = iwrb == null ? "Login failed" : iwrb.getLocalizedString(errorLocalizedKey, "Login failed");
			getLogger().log(Level.WARNING, error + ". Username: " + username + ", password: " + password, e);
			return new LoginResult(-1, error, errorLocalizedKey);
		}
	}

	@Override
	public String doUpdatePassword(
			String token,
			String newPassword,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) {
		if (StringUtil.isEmpty(newPassword)) {
			throw new BadRequest("Password can not be empty");
		}

		String toRemove = getSettings().getProperty("portal.remove_quotes_for_psw", StringUtil.getValue(Arrays.asList(CoreConstants.QOUTE_MARK, CoreConstants.QOUTE_SINGLE_MARK)));
		if (!StringUtil.isEmpty(toRemove)) {
			List<String> quotes = StringUtil.getValuesFromString(toRemove, CoreConstants.COMMA);
			if (!ListUtil.isEmpty(quotes)) {
				for (String quote: quotes) {
					if (StringUtil.isEmpty(quote)) {
						continue;
					}

					if (newPassword.startsWith(quote)) {
						newPassword = newPassword.substring(1);
					}
					if (newPassword.endsWith(quote)) {
						newPassword = newPassword.substring(0, newPassword.length() - 1);
					}
				}

				if (StringUtil.isEmpty(newPassword)) {
					throw new BadRequest("Password can not be empty");
				}
			}
		}

		validateUpdatePasswordToken(token);
		com.idega.user.data.User user = passwordTokenBusiness.completePasswordReset(
				token,
				newPassword
		);
		if (user == null) {
			throw new InternalServerError(
					"Failed changing password by token '"
						+ token
						+ "' and password '"
						+ newPassword
						+ "'"
			);
		}

		return Boolean.TRUE.toString();
	}

	private void validateUpdatePasswordToken(String token) {
		if (StringUtil.isEmpty(token)) {
			throw new BadRequest("Token can not be empty");
		}
		PasswordTokenEntity passwordToken = passwordTokenEntityDAO.findByToken(token);
		if (passwordToken == null) {
			throw new BadRequest("Token '"+token+"' is not found in database");
		}
		if (passwordToken.isExpired()) {
			throw new BadRequest("Token '"+token+"' is expired");
		}
	}

	@Override
	public String isUpdatePasswordLinkValid(
			String token,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) {
		validateUpdatePasswordToken(token);
		return "\"" + token + "\"";
	}

	private InputStream getStream(Object source) throws Exception {
		if (source instanceof ICFile) {
			return ((ICFile) source).getFileValue();
		} else if (source instanceof JCRItem) {
			return ((JCRItem) source).getInputStream();
		}
		return null;
	}

	@Override
	public Response getRepositoryFile(String identifier, String fileToken, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		if (StringUtil.isEmpty(identifier)) {
			return null;
		}

		String name = null;
		try {
			IWContext iwc = new IWContext(request, response, context);

			Object source = null;
			if (identifier.indexOf(CoreConstants.SLASH) == -1) {
				ICFileHome fileHome = (ICFileHome) IDOLookup.getHome(ICFile.class);
				ICFile file = fileHome.findByUUID(identifier);
				if (file != null && !StringUtil.isEmpty(fileToken) && fileToken.equals(file.getToken())) {
					source = file;
					name = file.getName();
				}

			} else if (repositoryAccessManager.hasPermission(iwc, identifier)) {
				JCRItem item = getRepositoryService().getRepositoryItemAsRootUser(identifier);
				source = item;
				name = item.getName();
			}
			InputStream stream = getStream(source);

			StreamingOutput fileStream = new StreamingOutput() {

				@Override
				public void write(OutputStream output) throws IOException, WebApplicationException {
					FileUtil.streamToOutputStream(stream, output);
				}

			};

			String type = MimeTypeUtil.resolveMimeTypeFromFileName(name);
			type = StringUtil.isEmpty(type) ? MediaType.APPLICATION_OCTET_STREAM : type;
			String object = MimeTypeUtil.MIME_TYPE_IOS_PASS.equals(type) ? "inline" : "attachment";
			return Response
	                .ok(fileStream, type)
	                .header("Content-Disposition", object.concat("; filename=\"" + name + "\""))
	                .build();
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error getting file " + identifier, e);
		}

		return null;
	}

	@Override
	public List<FileUploadResult> doUploadFilesToRepository(
			FormDataMultiPart form,
			HttpServletRequest request,
			HttpServletResponse response,
			ServletContext context
	) {
		if (form == null) {
			getLogger().warning("Unknown parameters, can not upload file(s)");
			return Arrays.asList(new FileUploadResult("FAILURE"));
		}

		String path = null;
		try {
			FormDataBodyPart uploadPath = form.getField("uploadPath");
			if (uploadPath != null) {
				path = uploadPath.getValueAs(String.class);
			}
			if (StringUtil.isEmpty(path)) {
				path = CoreConstants.WEBDAV_SERVLET_URI + CoreConstants.PUBLIC_PATH + CoreConstants.SLASH + UUID.randomUUID().toString() + CoreConstants.SLASH;
			} else {
				if (path.indexOf(CoreConstants.UNDEFINED) != -1) {
					String correctPath = getSettings().getProperty("portal.attachments_path", CoreConstants.PATH_FILES_ROOT.concat(CoreConstants.SLASH).concat(PortalConstants.ATTACHMENTS));
					correctPath = StringUtil.isEmpty(correctPath) ? UUID.randomUUID().toString() : correctPath;

					path = StringHandler.replace(path, CoreConstants.UNDEFINED, correctPath);
					String doubleSlash = CoreConstants.SLASH.concat(CoreConstants.SLASH);
					if (path.indexOf(doubleSlash) != -1) {
						path = StringHandler.replace(path, doubleSlash, CoreConstants.SLASH);
					}
				}
				if (!path.endsWith(CoreConstants.SLASH)) {
					path = path + CoreConstants.SLASH;
				}
				path = path + UUID.randomUUID().toString() + CoreConstants.SLASH;

				if (!path.endsWith(CoreConstants.SLASH)) {
					path = path + CoreConstants.SLASH;
				}
				if (!path.startsWith(CoreConstants.SLASH)) {
					path = CoreConstants.SLASH + path;
				}
				if (!path.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
					path = CoreConstants.WEBDAV_SERVLET_URI + path;
				}
			}

			IWContext iwc = new IWContext(request, response, context);
			User admin = iwc.getAccessController().getAdministratorUser();
			UploadAreaBean uploadHelper = ELUtil.getInstance().getBean(UploadAreaBean.BEAN_NAME);
			Long maxSize = uploadHelper.getMaxFileSize(iwc);
			RepositoryService repositoryService = getRepositoryService();

			char[] exceptions = new char[] {'-', '_', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'};
			boolean addPrefix = iwc.getApplicationSettings().getBoolean("blue_imp_upload.add_prefix", false);
			int index = 0;
			InputStream stream = null;
			FormDataBodyPart filePart = null;
			List<FileUploadResult> results = new ArrayList<>();
			while ((filePart = form.getField("file_".concat(String.valueOf(index)))) != null) {
				try {
					ContentDisposition cd = filePart.getContentDisposition();
					String originalName = cd.getFileName();

					Long fileSize = null;
					FormDataBodyPart fileSizeInfo = form.getField("fileSize_".concat(String.valueOf(index)));
					if (fileSizeInfo != null) {
						String sizeInfo = fileSizeInfo.getEntityAs(String.class);
						if (StringHandler.isNumeric(sizeInfo)) {
							fileSize = Long.valueOf(sizeInfo);
						}
					}

					index++;

					if (maxSize != null && fileSize != null && fileSize > maxSize) {
						getLogger().warning("File " + filePart + " is too big: max size: " + maxSize + ", file's size: " + fileSize);
						results.add(new FileUploadResult("FAILURE", null, originalName, null));
						continue;
					}

					MediaType type = filePart.getMediaType();
					stream = filePart.getEntityAs(InputStream.class);

					String fileName = originalName;
					fileName = StringHandler.stripNonRomanCharacters(fileName, exceptions);
					if (addPrefix) {
						fileName = UUID.randomUUID().toString().concat(CoreConstants.UNDER).concat(fileName);
					}
					String pathAndName = path + fileName;
					boolean success = repositoryService.uploadFile(path, fileName, type == null ? MimeTypeUtil.resolveMimeTypeFromFileName(fileName) : type.toString(), stream);
					if (!success) {
						getLogger().warning("Failed to upload file " + filePart + " to " + path);
						results.add(new FileUploadResult("FAILURE", fileName, originalName, pathAndName));
						continue;
					}

					fileSize = (fileSize == null || fileSize <= 0) ? repositoryService.getLength(pathAndName, admin) : fileSize;

					Map<String, Object> uploadResult = uploadHelper.getFileResponse(fileName, fileSize == null ? 0 : fileSize, pathAndName, -1, false);
					if (MapUtil.isEmpty(uploadResult)) {
						getLogger().warning("Failed to upload result for file " + filePart + " to " + path);
						results.add(new FileUploadResult("FAILURE", fileName, originalName, pathAndName));
						continue;
					}

					FileUploadResult result = new FileUploadResult("OK");
					result.setOriginalName(originalName);
					result.setName((String) uploadResult.get("name"));
					result.setSize((Long) uploadResult.get("size"));
					result.setUrl((String) uploadResult.get("url"));
					result.setThumbnail_url((String) uploadResult.get("thumbnail_url"));
					result.setDelete_url((String) uploadResult.get("delete_url"));
					result.setDelete_type((String) uploadResult.get("delete_type"));
					result.setMessage(CoreConstants.EMPTY);
					results.add(result);
				} finally {
					IOUtil.close(stream);
				}
			}

			if (ListUtil.isEmpty(results)) {
				getLogger().warning("No files were uploaded " + form);
				return Arrays.asList(new FileUploadResult("FAILURE"));
			}

			return results;
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error uploading file " + form + " to " + path, e);
		}

		return Arrays.asList(new FileUploadResult("FAILURE"));
	}

	@Override
	public Result isValidUserName(UserAccount userAccount, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		Result error = new Result(Status.BAD_REQUEST.getStatusCode(), Boolean.FALSE.toString());
		Result success = new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
		if (userAccount == null) {
			getLogger().warning("Account information is not provided");
			return error;
		}

		String userName = userAccount.getUsername();
		if (StringUtil.isEmpty(userName)) {
			getLogger().warning("Username is not provided. Received data: " + userAccount);
			return error;
		}

		try {
			LoginTableHome loginInfo = (LoginTableHome) IDOLookup.getHome(LoginTable.class);
			LoginTable login = loginInfo.findByLogin(userName);
			if (login == null) {
				return success;
			}

			String personalId = userAccount.getPersonalId();
			if (StringUtil.isEmpty(personalId)) {
				getLogger().warning("Personal ID is not provided. Received data: " + userAccount);
				return error;
			}

			personalId = personalId.trim();
			personalId = StringHandler.getNumbersOnly(personalId);

			com.idega.user.data.User user = login.getUser();
			String loginPersonalId = user == null ? null : user.getPersonalID();
			if (StringUtil.isEmpty(loginPersonalId)) {
				getLogger().warning("Personal ID not found for user " + user + " (ID: " + (user == null ? "unknown" : user.getId()) + ") with username '" + userName +
						"'. Received data: " + userAccount);
				return error;
			}

			if (personalId.equals(loginPersonalId)) {
				if (user != null) {
					success.setValue(user.getUniqueId());
				}
				return success;
			}

			UserBusiness userBusiness = getServiceInstance(UserBusiness.class);
			if (userBusiness.validatePersonalId(loginPersonalId)) {
				getLogger().warning("Personal ID ('" + loginPersonalId + "') of found user " + user + " (ID: " + (user == null ? "unknown" : user.getId()) +
						") is valid and not equals to provided one ('" + personalId + "'). Username '" + userName +
						"' is taken and can not be used by other user. Received data: " + userAccount);
				return error;
			}

			String providedName = userAccount.getName();
			String name = user == null ? null : user.getName();
			if (!StringUtil.isEmpty(providedName) && !StringUtil.isEmpty(name) && providedName.equalsIgnoreCase(name)) {
				getLogger().info("Personal ID ('" + loginPersonalId + "') of found user " + user + " (ID: " + (user == null ? "unknown" : user.getId()) +
						") is invalid and not equals to provided one ('" + personalId +
						"'). But names (provided: '" + providedName + "', found: '" + name + "') are the same: considering username '" + userName +
						"' as not taken and can be used by the same person. Received data: " + userAccount);
				if (user != null) {
					success.setValue(user.getUniqueId());
				}
				return success;
			}

			getLogger().warning("Username '" + userName + "' is already taken by " + user + " (ID: " + (user == null ? "unknown" : user.getId()) +
					". Received data: " + userAccount);
			return error;
		} catch (FinderException e) {
			return new Result(Status.OK.getStatusCode(), Boolean.TRUE.toString());
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error while checking if user name is valid: " + userName + ". Received data: " + userAccount, e);
		}

		return error;
	}

}