package com.idega.portal;

import com.idega.util.CoreConstants;

public class PortalConstants {

	public static final String	IW_BUNDLE_IDENTIFIER = "com.idega.portal",

								CLIENT_ID = "egov_dashboard",
								CLIENT_SECRET = "egov_dashboard",

								QUALIFIER_PORTAL= "portalService",
								QUALIFIER_APP = "appService",
								QUALIFIER_USER = "userService",

								WIFE = "wife",
								HUSBAND = "husband",
								SPOUSE = "spouse",
								CHILD = "child",
								SON = "son",
								DAUGHTER = "daughter",

								CASE_MUST_BE_READ = "case_must_be_read",
								CASE_READ_BY = "case_read_by",

								PROPERTY_FOOTER_MUNICIPALITY = "footer.municipality",
								PROPERTY_FOOTER_ADDRESS = "footer.address",
								PROPERTY_FOOTER_PHONE_NUMBER = "footer.phone_number",
								PROPERTY_FOOTER_FAX_NUMBER = "footer.fax_number",
								PROPERTY_FOOTER_EMAIL_ADDRESS = "footer.email_address",
								PROPERTY_FOOTER_WORKING_HOURS = "footer.working_hours",

								PROPERTY_PORTAL_OAUTH_CLIENTS = "portal_oauth_clients",
								PROPERTY_PORTAL_LOCALIZER_BUNDLE_ID = CoreConstants.PROPERTY_PORTAL_LOCALIZER_BUNDLE_ID,

								METADATA_CONTACT_BY_EMAIL = "CONTACT_BY_EMAIL",
								METADATA_CONTACT_BY_SMS = "CONTACT_BY_SMS",
								METADATA_CONTACT_BY_MY_MESSAGES = "CONTACT_BY_MY_MESSAGES",

								METADATA_EULA_AGREED = "eula_agreed",

								PAYMENT = "/payment",
								INITIALIZATION = "/initialization",
								AUTHORIZATION = "/authorization",
								STATUS = "/status",
								CALLBACK = "/cancel",
								SUCCESS = "/success",
								STORE = "/store",
								PAYMENT_UNIQUE_ID = "payment_unique_id",

								METADATA_PRICE = "price",
								MERCHANT_TYPE = "MERCHANT_TYPE",
								MERCHANT_ID = "MERCHANT_ID",
								MERCHANT_CURRENCY = "MERCHANT_CURRENCY",
								CREDIT_CARD_AUTHORIZATION_ENTRY = "credit_card_auth_entry",
								CREDIT_CARD_AMOUNT = "credit_card_amount",

								ATTACHMENTS = "attachments";

	public static final Integer MIN_PASSWORD_LENGTH = 6;

	public static final String APP_PROPERTY_USE_2_STEP_AUTH = "portal.use_2_step_auth";

	public static final String APP_PROPERTY_2_STEP_AUTH_VALIDITY = "portal.2_step_auth_validity";

	public static final String APP_PROPERTY_2_STEP_AUTH_SELECTABLE = "portal.2_step_auth_selectable";

	public static final String METADATA_USER_2_STEP_AUTH_SELECTED = "portal.user_2_step_auth_selected";

}