package com.idega.portal.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Result implements Serializable {

	private static final long serialVersionUID = -4514692377064102150L;

	private Integer status;

	private String name;

	private String value;

	public Result() {
		super();
	}

	public Result(Integer status) {
		this();

		this.status = status;
	}

	public Result(Integer status, String name) {
		this(status);

		this.name = name;
	}

	public Result(String name, String value) {
		this();

		this.name = name;
		this.value = value;
	}

	public Result(Integer status, String name, String value) {
		this();

		this.status = status;
		this.name = name;
		this.value	= value;
	}

	public Integer getStatus() {
		return status;
	}

	public void setStatus(Integer status) {
		this.status = status;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return "Name: " + getName() + ", value: " + getValue() + ", HTTP status code: " + getStatus();
	}

}