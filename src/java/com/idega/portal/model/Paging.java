package com.idega.portal.model;

import java.io.Serializable;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Paging implements Serializable {

	private static final long serialVersionUID = 5244123040583456531L;

	private Long totalCount = Long.valueOf(0);

	private Integer pageSize = 100;

	private Integer page = 0;

	private List<String> variables;

	public Long getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(Long totalCount) {
		this.totalCount = totalCount;
	}

	public Integer getPageSize() {
		return pageSize;
	}

	public void setPageSize(Integer pageSize) {
		this.pageSize = pageSize;
	}

	public Integer getPage() {
		return page;
	}

	public void setPage(Integer page) {
		this.page = page;
	}

	public List<String> getVariables() {
		return variables;
	}

	public void setVariables(List<String> variables) {
		this.variables = variables;
	}

	@Override
	public String toString() {
		return "Page: " + getPage() + ", size: " + getPageSize() + ", total count: " + getTotalCount();
	}

}