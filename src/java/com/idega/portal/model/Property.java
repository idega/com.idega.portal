package com.idega.portal.model;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Property<T extends Serializable, V> implements Serializable {

	private static final long serialVersionUID = 6585673302658450347L;

	private T name;

	private V value;

	public Property() {
		super();
	}

	public Property(T name) {
		this();

		this.name = name;
	}

	public Property(T name, V value) {
		this(name);

		this.value = value;
	}

	public T getName() {
		return name;
	}

	public void setName(T name) {
		this.name = name;
	}

	public V getValue() {
		return value;
	}

	public void setValue(V value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return "Name: " + getName() + ", value: " + getValue();
	}

}