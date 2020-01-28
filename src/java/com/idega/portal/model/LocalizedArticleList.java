package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.restful.bean.JAXBNatural;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class LocalizedArticleList implements Serializable, JAXBNatural {
	private static final long serialVersionUID = -4118175931841165740L;
	
	private List<LocalizedArticle> articles;

	public List<LocalizedArticle> getArticles() {
		return articles;
	}

	public void setArticles(List<LocalizedArticle> articles) {
		this.articles = articles;
	}
}
