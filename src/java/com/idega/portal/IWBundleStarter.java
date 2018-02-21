/*
 * $Id: IWBundleStarter.java,v 1.2 2008/01/30 14:32:04 tryggvil Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.security.oauth2.provider.ClientDetails;
import org.springframework.security.oauth2.provider.client.BaseClientDetails;
import org.springframework.security.oauth2.provider.client.JdbcClientDetailsService;

import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.portal.gateway.PortalGateway;
import com.idega.portal.model.Property;
import com.idega.portal.security.SecurityUtil;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class IWBundleStarter implements IWBundleStartable {

	private static final Logger LOGGER = Logger.getLogger(IWBundleStarter.class.getName());

	@Autowired(required=false)
	@Qualifier("clientDetails")
	private JdbcClientDetailsService clientDetailsService;

	private JdbcClientDetailsService getClientDetailsService() {
		if (this.clientDetailsService == null) {
			ELUtil.getInstance().autowire(this);
		}

		return clientDetailsService;
	}

	private void doCreateOAuthClient(String clientId, String clientSecret, Integer accessTokenValiditySeconds, Integer refreshTokenValiditySeconds) {
		try {
			JdbcClientDetailsService clientDetailsService = getClientDetailsService();
			if (clientDetailsService == null) {
				return;
			}

			ClientDetails clientDetails = null;
			try {
				clientDetails = clientDetailsService.loadClientByClientId(clientId);
			} catch (Exception e) {}
			if (clientDetails == null) {
				BaseClientDetails details = new BaseClientDetails(
						clientId,
						null,
						"read,write,trust",
						"password,authorization_code,refresh_token,implicit",
						"ROLE_APP"
				);
				details.setAccessTokenValiditySeconds(accessTokenValiditySeconds);
				details.setRefreshTokenValiditySeconds(refreshTokenValiditySeconds);
				details.setClientSecret(clientSecret);
				getClientDetailsService().addClientDetails(details);
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error creating OAuth client " + clientId + " with secret " + clientSecret, e);
		}
	}

	private void doCreateOAuthClients(IWMainApplicationSettings settings) {
		if (settings == null) {
			return;
		}

		String clientsInfo = null;
		try {
			clientsInfo = settings.getProperty("portal_oauth_clients");	//	i.e. portal_oauth_clients=egov_dashboard,egov_dashboard,30000,30000;egov_portal,egov_portal;
			if (StringUtil.isEmpty(clientsInfo)) {
				return;
			}

			String[] info = clientsInfo.split(CoreConstants.SEMICOLON);
			if (ArrayUtil.isEmpty(info)) {
				return;
			}

			for (String clientInfo: info) {
				if (StringUtil.isEmpty(clientInfo)) {
					continue;
				}

				String[] allClientInfo = clientInfo.split(CoreConstants.COMMA);
				if (ArrayUtil.isEmpty(allClientInfo) || allClientInfo.length < 2) {
					continue;
				}

				String clientId = allClientInfo[0];
				String clientSecret = allClientInfo[1];
				Integer accessTokenValiditySeconds = allClientInfo.length > 2 && StringHandler.isNumeric(allClientInfo[2]) ? Integer.valueOf(allClientInfo[2]) : null;
				Integer refreshTokenValiditySeconds = allClientInfo.length > 3 && StringHandler.isNumeric(allClientInfo[3]) ? Integer.valueOf(allClientInfo[3]) : null;
				doCreateOAuthClient(clientId, clientSecret, accessTokenValiditySeconds, refreshTokenValiditySeconds);
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error creating OAuth client(s): " + clientsInfo, e);
		}
	}

	private void doRegisterAccessArtifacts() {
		//	Portal
		SecurityUtil.getInstance().doRegisterAccessArtifact(
				PortalGateway.PORTAL,
				Arrays.asList(
						new Property<String, List<String>>(PortalGateway.SETTINGS, SecurityUtil.getInstance().getAllRoles()),
						new Property<String, List<String>>(PortalGateway.ACCOUNT, SecurityUtil.getInstance().getAllRoles()),
						new Property<String, List<String>>(PortalGateway.MENUS, SecurityUtil.getInstance().getAllRoles())
				)
		);

		//	User
		SecurityUtil.getInstance().doRegisterAccessArtifact(
				PortalGateway.USER,
				Arrays.asList(
						new Property<String, List<String>>(PortalGateway.PROFILE,SecurityUtil.getInstance().getAllRoles()),
						new Property<String, List<String>>(PortalGateway.PROFILE + PortalGateway.UPDATE, SecurityUtil.getInstance().getAllRoles()),
						new Property<String, List<String>>(PortalGateway.PROFILE + PortalGateway.PICTURE + PortalGateway.UPDATE, SecurityUtil.getInstance().getAllRoles())
				)
		);
	}

	@Override
	public void start(IWBundle starterBundle) {
		doCreateOAuthClients(starterBundle.getApplication().getSettings());

		doRegisterAccessArtifacts();
	}

	@Override
	public void stop(IWBundle starterBundle) {
	}

}