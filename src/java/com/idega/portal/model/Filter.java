package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Filter implements Serializable {

	private static final long serialVersionUID = -7912045253154873040L;

	private List<DataElement> dataToLoad = null;

	private Boolean loadDataFromLegacyUser = Boolean.FALSE;

	public List<DataElement> getDataToLoad() {
		return dataToLoad;
	}

	public void setDataToLoad(List<DataElement> dataToLoad) {
		this.dataToLoad = dataToLoad;
	}

	public Boolean getLoadDataFromLegacyUser() {
		return loadDataFromLegacyUser;
	}

	public void setLoadDataFromLegacyUser(Boolean loadDataFromLegacyUser) {
		this.loadDataFromLegacyUser = loadDataFromLegacyUser;
	}

}