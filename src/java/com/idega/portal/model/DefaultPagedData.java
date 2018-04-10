package com.idega.portal.model;

import java.util.ArrayList;
import java.util.List;

import com.idega.presentation.paging.PagedDataCollection;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;

public abstract class DefaultPagedData extends Paging {

	private static final long serialVersionUID = -4434862515727338445L;

	private List<? extends Object> data;

	public DefaultPagedData() {
		super();
	}

	public <T extends Object> DefaultPagedData(PagedDataCollection<T> pd) {
		this(pd == null || ListUtil.isEmpty(pd.getCollection()) ? null : new ArrayList<>(pd.getCollection()), pd == null ? null :  pd.getTotalCount());
	}

	public <T extends Object> DefaultPagedData(List<T> data, Long totalCount) {
		this();

		this.data = data;
		setTotalCount(totalCount);
	}

	public <T extends Object> DefaultPagedData(List<T> data) {
		this();

		this.data = data;
		if (data != null) {
			setTotalCount(Long.valueOf(data.size()));
		}
	}

	public <T extends Object> DefaultPagedData(List<T> data, Long totalCount, Integer pageSize, Integer page) {
		this();

		this.data = data;
		setTotalCount(totalCount);
		setPageSize(pageSize);
		setPage(page);
	}

	public List<? extends Object> getData() {
		return data;
	}

	public <T extends Object> void setData(List<T> data) {
		this.data = data;
	}

	@Override
	public String toString() {
		return ListUtil.isEmpty(data) ? CoreConstants.MINUS : data.toString();
	}

}