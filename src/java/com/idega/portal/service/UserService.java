package com.idega.portal.service;

import java.io.InputStream;

import com.idega.portal.model.Filter;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.sun.jersey.core.header.FormDataContentDisposition;

public interface UserService {

	public UserProfile getCitizenProfile(Filter filter);

	public Result setProfile(UserProfile profile);

	public Result setProfilePicture(InputStream stream, FormDataContentDisposition info, String personalId);

}