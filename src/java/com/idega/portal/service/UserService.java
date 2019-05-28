package com.idega.portal.service;

import java.io.InputStream;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.idega.portal.model.Filter;
import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.sun.jersey.core.header.FormDataContentDisposition;

public interface UserService {

	public UserProfile getCitizenProfile(Filter filter, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setProfile(UserProfile profile, HttpServletRequest request, HttpServletResponse response, ServletContext context);

	public Result setProfilePicture(InputStream stream, FormDataContentDisposition info, String personalId, HttpServletRequest request, HttpServletResponse response, ServletContext context);

}