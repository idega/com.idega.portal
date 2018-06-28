package com.idega.portal.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class PortalMenu implements Serializable {

	private static final long serialVersionUID = -4956615420250161500L;

	private String name, url, icon, defaultName;

	/**
	 * Possible icons:
	 *
	 * icon-search
	 * icon-user
	 * icon-home
	 * icon-email
	 * icon-paper
	 * icon-news
	 * icon-discus
	 * icon-location
	 * icon-card
	 * icon-tag
	 * icon-close
	 * icon-info
	 * icon-plus
	 * icon-star
	 * icon-pin
	 * icon-arrow-right
	 * icon-settings
	 */

	private Integer order = 0;

	private boolean title, divider;

	private List<PortalMenu> children;

	private Badge badge;

	private Boolean openInNewWindow;

	public PortalMenu() {
		super();
	}

	public PortalMenu(String name, String defaultName, String url, String icon) {
		this();

		this.name = name;
		this.defaultName = defaultName;
		this.url = url;
		this.icon = icon;

	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDefaultName() {
		return defaultName;
	}

	public void setDefaultName(String defaultName) {
		this.defaultName = defaultName;
	}

	public List<PortalMenu> getChildren() {
		return children;
	}

	public void setChildren(List<PortalMenu> children) {
		this.children = children;
	}

	public void addChild(PortalMenu child) {
		if (child == null) {
			return;
		}

		if (children == null) {
			children = new ArrayList<>();
		}

		children.add(child);
	}

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public Badge getBadge() {
		return badge;
	}

	public void setBadge(Badge badge) {
		this.badge = badge;
	}

	public Boolean getTitle() {
		return title;
	}

	public void setTitle(Boolean title) {
		this.title = title;
	}

	public Boolean getDivider() {
		return divider;
	}

	public void setDivider(Boolean divider) {
		this.divider = divider;
	}

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public Boolean getOpenInNewWindow() {
		return openInNewWindow;
	}

	public void setOpenInNewWindow(Boolean openInNewWindow) {
		this.openInNewWindow = openInNewWindow;
	}


}