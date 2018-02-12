/*
 * $Id: IWBundleStarter.java,v 1.2 2008/01/30 14:32:04 tryggvil Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.portal;

import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;

public class IWBundleStarter implements IWBundleStartable {

	@Override
	public void start(IWBundle starterBundle) {}

	@Override
	public void stop(IWBundle starterBundle) {
	}

}