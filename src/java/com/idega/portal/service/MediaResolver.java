package com.idega.portal.service;

import com.idega.core.file.util.FileInfo;

public interface MediaResolver {

	public FileInfo getFileInfo(String fileUniqueId);

}