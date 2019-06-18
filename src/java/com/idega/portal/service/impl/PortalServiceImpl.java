package com.idega.portal.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response.Status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.article.bean.ArticleItemBean;
import com.idega.block.article.data.ArticleEntity;
import com.idega.block.article.data.dao.ArticleDao;
import com.idega.core.accesscontrol.business.LoginDBHandler;
import com.idega.core.accesscontrol.data.LoginTable;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.contact.data.Email;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.portal.PortalConstants;
import com.idega.portal.model.Article;
import com.idega.portal.model.ArticleList;
import com.idega.portal.model.LanguageData;
import com.idega.portal.model.Localization;
import com.idega.portal.model.Localizations;
import com.idega.portal.model.PortalMenu;
import com.idega.portal.model.PortalSettings;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserAccount;
import com.idega.portal.security.SecurityUtil;
import com.idega.portal.service.LocalizationService;
import com.idega.portal.service.PortalService;
import com.idega.portal.service.PortalSettingsResolver;
import com.idega.presentation.IWContext;
import com.idega.user.business.UserBusiness;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.WebUtil;
import com.idega.util.text.Name;

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
			LoginTable login = LoginDBHandler.getUserLoginByUserName(account.getUsername());
			if (login != null) {
				account.setErrorMessage(iwrb.getLocalizedString("account.please_choose_different_user_name", "Please choose different username"));
				return account;
			}

			com.idega.user.data.User user = null;

			String firstName = null;
			String middleName = null;
			String lastName = null;
			String displayName = null;

			if (!StringUtil.isEmpty(account.getName())) {
				Name userNames = new Name(account.getName());
				firstName = userNames.getFirstName();
				middleName = userNames.getMiddleName();
				lastName = userNames.getLastName();
				displayName = account.getName();
			}

			if (!getUserBusiness().validatePersonalId(account.getPersonalId(), getCurrentLocale())) {
				account.setErrorMessage(iwrb.getLocalizedString("account.provide_valid_personal_id", "Please provide valid personal ID"));
				return account;
			}

			if (!StringUtil.isEmpty(account.getUuid())) {
				user = getUserBusiness().update(
						null,
						account.getUuid(),
						null,
						firstName,
						middleName,
						lastName,
						displayName,
						account.getPersonalId(),
						account.getEmail(),
						null,
						account.getUsername(),
						account.getPassword());
			} else {
				user = getUserBusiness().createUserWithLogin(
						firstName,
						middleName,
						lastName,
						account.getPersonalId(),
						displayName,
						null,
						null,
						null,
						null,
						account.getUsername(),
						account.getPassword(),
						Boolean.TRUE,
						IWTimestamp.RightNow(),
						5000,
						Boolean.FALSE,
						Boolean.TRUE,
						Boolean.FALSE,
						null);
			}

			if (user == null) {
				account.setErrorMessage(iwrb.getLocalizedString("account.failed_to_create_account", "Sorry, some error occurred - failed to create account. Please try later"));
				return account;
			}

			if (!StringUtil.isEmpty(account.getEmail())) {
				Email email = getUserBusiness().updateUserMail(user, account.getEmail());
				if (email == null) {
					getLogger().log(
							Level.WARNING,
							"Error updating email: " + account.getEmail()
									+ " for user (ID: "
									+ user.getPrimaryKey().toString() + ")");
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
	public String doRemindPassword(String ssn, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		return "Unimplemented";
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
			article.setBody(articleItemBean.getBody());
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error loading article at " + uri, e);
		}
		return article;
	}

	@Override
	public ArticleList getArticlesByCategory(String category, HttpServletRequest request, HttpServletResponse response, ServletContext context) {
		if (category == null) {
			return null;
		}

		List<String> categories = new ArrayList<String>();
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

		List<ArticleItemBean> articleBeans = new ArrayList<ArticleItemBean>();
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
		List<Article> art = new ArrayList<Article>();
		result.setArticles(art);
		for (ArticleItemBean article: articleBeans) {
			art.add(new Article(article));
		}
		return result;
	}

}