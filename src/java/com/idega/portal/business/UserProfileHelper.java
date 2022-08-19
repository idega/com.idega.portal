package com.idega.portal.business;

import com.idega.portal.model.Result;
import com.idega.portal.model.UserProfile;
import com.idega.presentation.IWContext;
import com.idega.user.data.bean.User;

public interface UserProfileHelper {

	public Result setProfile(IWContext iwc, UserProfile profile, User currentUser);

	public com.idega.user.data.User getUserByPersonalId(String personalId);

}