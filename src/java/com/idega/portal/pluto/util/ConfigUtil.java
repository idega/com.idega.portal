package com.idega.portal.pluto.util;

import java.io.InputStream;
import org.apache.pluto.driver.services.impl.resource.ResourceConfigReader;


public class ConfigUtil {

	public static InputStream getConfigFileInputStream(){
		return ConfigUtil.class.getResourceAsStream(ResourceConfigReader.CONFIG_FILE);
	}

}
