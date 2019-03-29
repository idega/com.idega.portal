package com.idega.portal.exception;

import java.sql.Timestamp;
import java.util.HashMap;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.Provider;

import com.google.gson.Gson;

@Provider
public class ExceptionMapper implements javax.ws.rs.ext.ExceptionMapper<Throwable> {

    public Response toResponse(Throwable exception) {
    	if(exception instanceof WebApplicationException) {
    		WebApplicationException wex = (WebApplicationException) exception;
    		return wex.getResponse();
    	}
    	
    	HashMap<String, String> info = new HashMap<>();
		info.put("error", "Unexpected error encountered");
		info.put("time", new Timestamp(System.currentTimeMillis()).toString());
		
		String content = new Gson().toJson(info);
    	Response response = Response.status(Response.Status.INTERNAL_SERVER_ERROR)
    			.entity(content)
    			.type(MediaType.APPLICATION_JSON)
    			.build();
    	return response;
    }

}
