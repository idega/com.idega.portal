/*
 * $Id: IWPortletPreferenes.java,v 1.2 2007/04/22 14:59:25 eiki Exp $
 * Created on 12.4.2006 in project com.idega.portal
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal.util;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.portlet.Portlet;
import javax.portlet.PortletPreferences;
import javax.portlet.ReadOnlyException;
import javax.portlet.ValidatorException;


/**
 * <p>
 * TODO tryggvil Describe Type IWPortletPreferenes
 * </p>
 *  Last modified: $Date: 2007/04/22 14:59:25 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.2 $
 */
public class IWPortletPreferenes implements PortletPreferences {

	private Hashtable preferences;
	
	
	/**
	 * @param portlet 
	 * @param context 
	 * 
	 */
	public IWPortletPreferenes(FacesContext context, Portlet portlet) {
		super();
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#isReadOnly(java.lang.String)
	 */
	public boolean isReadOnly(String arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#getValue(java.lang.String, java.lang.String)
	 */
	public String getValue(String key, String defaultValue) {
		
		String value = (String) getPreferences().get(key);
		if(value!=null){
			return value;
		}
		return defaultValue;
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#getValues(java.lang.String, java.lang.String[])
	 */
	public String[] getValues(String arg0, String[] arg1) {
		return (String[]) getPreferences().values().toArray(new String[0]);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#setValue(java.lang.String, java.lang.String)
	 */
	public void setValue(String key, String value) throws ReadOnlyException {
		getPreferences().put(key,value);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#setValues(java.lang.String, java.lang.String[])
	 */
	public void setValues(String key, String[] values) throws ReadOnlyException {
		getPreferences().put(key,values);
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#getNames()
	 */
	public Enumeration getNames() {
		return getPreferences().keys();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#getMap()
	 */
	public Map getMap() {
		return getPreferences();
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#reset(java.lang.String)
	 */
	public void reset(String arg0) throws ReadOnlyException {
		// TODO Auto-generated method stub
	}

	/* (non-Javadoc)
	 * @see javax.portlet.PortletPreferences#store()
	 */
	public void store() throws IOException, ValidatorException {
		// TODO Auto-generated method stub
	}

	
	/**
	 * @return Returns the preferences.
	 */
	Hashtable getPreferences() {
		if(preferences==null){
			preferences=new Hashtable();
		}
		return preferences;
	}

	
	/**
	 * @param preferences The preferences to set.
	 */
	void setPreferences(Hashtable preferences) {
		this.preferences = preferences;
	}
}
