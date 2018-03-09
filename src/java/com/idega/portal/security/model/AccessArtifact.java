package com.idega.portal.security.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.portal.model.Property;
import com.idega.portal.security.SecurityUtil;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;

public class AccessArtifact {

	private static final Logger LOGGER = Logger.getLogger(AccessArtifact.class.getName());

	/*	Example:
	 *	Name
	 *	Property<String, List<String>>(
	 *		String:			name of URI
	 *		List<String>:	role(s) that can call this URI
	 *	)
	 */

	//	Component's path
	private String name;

	//	URI -> roles
	private List<Property<String, List<String>>> accesses = new ArrayList<>();

	public AccessArtifact(String name, List<Property<String, List<String>>> accesses) {
		this.name = name;
		this.accesses = accesses;
	}

	public List<Property<String, List<String>>> getAccesses() {
		return accesses;
	}

	public void setAccesses(List<Property<String, List<String>>> accesses) {
		this.accesses = accesses;
	}

	public void addAccess(Property<String, List<String>> access) {
		if (access == null) {
			return;
		}

		if (accesses == null) {
			accesses = new ArrayList<>();
		}

		accesses.add(access);
	}

	public String getName() {
		return name;
	}

	private Map<String, List<String>> getAllAccesses() {
		if (ListUtil.isEmpty(this.accesses)) {
			return null;
		}

		Map<String, List<String>> allAccess = new HashMap<>();
		for (Property<String, List<String>> access: this.accesses) {
			if (access == null) {
				continue;
			}

			String name = access.getName();
			if (StringUtil.isEmpty(name)) {
				continue;
			}

			Property<String, List<String>> accesesses = getAccesses(name);
			if (accesesses == null || ListUtil.isEmpty(accesesses.getValue())) {
				continue;
			}

			List<String> accessesByUri = allAccess.get(name);
			if (accessesByUri == null) {
				accessesByUri = new ArrayList<>();
				allAccess.put(name, accessesByUri);
			}
			accessesByUri.addAll(accesesses.getValue());
		}

		return allAccess;
	}

	private static final Property<String, List<String>> getAccesses(String uri) {
		if (StringUtil.isEmpty(uri)) {
			return null;
		}

		List<AccessArtifact> artifacts = SecurityUtil.getInstance().getAllAccessArtifacts();
		if (ListUtil.isEmpty(artifacts)) {
			return null;
		}

		String artifactsNames = CoreConstants.EMPTY;
		Set<String> accesses = new HashSet<>();
		for (AccessArtifact artifact: artifacts) {
			String artifactName = artifact.getName();

			for (Property<String, List<String>> access: artifact.accesses) {
				if (access == null) {
					continue;
				}

				String accessName = access.getName();
				if (accessName == null) {
					continue;
				}

				if (!accessName.startsWith(artifactName)) {
					accessName = artifactName.concat(accessName);
				}

				if (uri.equals(accessName) && !ListUtil.isEmpty(access.getValue())) {
					accesses.addAll(access.getValue());

					if (artifactsNames.indexOf(uri) == -1) {
						artifactsNames = artifactsNames.concat(uri);
					} else if (artifactsNames.indexOf(artifactName) == -1) {
						artifactsNames = artifactsNames.concat(artifactName);
					}
				}
			}
		}

		return new Property<String, List<String>>(artifactsNames, new ArrayList<>(accesses));
	}

	public static final Property<String, List<String>> getRoles(IWMainApplicationSettings settings, String uri) {
		if (StringUtil.isEmpty(uri)) {
			LOGGER.warning("URI is not provided");
			return null;
		}

		Property<String, List<String>> accesses = getAccesses(uri);
		if (accesses == null || ListUtil.isEmpty(accesses.getValue())) {
			LOGGER.warning("Failed to find accesses for URI " + uri);
			return null;
		}

		List<String> roles = null;
		try {
			String rolesProp = settings.getProperty(
					"ws.".concat(accesses.getName()).concat(CoreConstants.UNDER).concat(uri),
					accesses == null ? null : ListUtil.convertListOfStringsToCommaseparatedString(accesses.getValue())
			);
			if (StringUtil.isEmpty(rolesProp)) {
				return null;
			}

			roles = ListUtil.convertCommaSeparatedStringToList(rolesProp);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting roles for " + uri, e);
		}

		return new Property<String, List<String>>(accesses.getName(), roles == null ? accesses.getValue() : roles);
	}

	@Override
	public String toString() {
		return getName() + ": " + getAllAccesses();
	}

}