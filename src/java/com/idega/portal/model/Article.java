package com.idega.portal.model;

import java.io.Serializable;
import java.util.Base64;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.article.bean.ArticleItemBean;

@XmlRootElement
public class Article implements Serializable {

	private static final long serialVersionUID = -4033780636343840369L;
	private String title,
			body,
			bodyRaw,
			url,
			teaser;

	@XmlElement
	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	@XmlElement
	public String getTeaser() {
		return teaser;
	}

	public void setTeaser(String teaser) {
		this.teaser = teaser;
	}

	public Article() {
		super();
	}

	public Article(ArticleItemBean article) {
		super();

		this.title = article.getHeadline();
		String body = article.getBody();
		if (body != null) {
			this.bodyRaw = body;
			this.body = Base64.getEncoder().encodeToString(body.getBytes());
		}
		this.url = article.getResourcePath();
		String teaser = article.getTeaser();
		if (teaser != null) {
			this.teaser = teaser;
		}
	}

	@XmlElement
	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@XmlElement
	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	@XmlElement
	public String getBodyRaw() {
		return bodyRaw;
	}

	public void setBodyRaw(String bodyRaw) {
		this.bodyRaw = bodyRaw;
	}

}