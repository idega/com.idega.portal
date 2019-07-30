package com.idega.portal.model;

import java.io.Serializable;
import java.util.Base64;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.article.bean.ArticleItemBean;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Article implements Serializable {

	private static final long serialVersionUID = -4033780636343840369L;
	private String title, 
			body, 
			url,
			teaser;

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

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
		if (article.getBody() != null) {
			this.body = Base64.getEncoder().encodeToString(article.getBody().getBytes());
		}
		this.url = article.getResourcePath();
		String teaser = article.getTeaser();
		if(teaser != null) {
			this.teaser = teaser;
		}
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

}