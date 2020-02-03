package com.idega.portal.model;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.idega.block.article.bean.ArticleItemBean;
import com.idega.restful.bean.JAXBNatural;

@XmlRootElement
public class LocalizedArticle extends Article implements JAXBNatural {

	private static final long serialVersionUID = -8185357128248745429L;
	
	private String locale;
	
	public LocalizedArticle() {
		super();
	}
	
	public LocalizedArticle(ArticleItemBean article) {
		super(article);
		this.locale = article.getLanguage();
	}

	@XmlElement
	public String getLocale() {
		return locale;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}
	


}