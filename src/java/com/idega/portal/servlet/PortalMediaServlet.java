package com.idega.portal.servlet;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.context.support.WebApplicationContextUtils;

import com.idega.block.media.business.MediaBusiness;
import com.idega.core.file.business.ICFileSystemFactory;
import com.idega.core.file.data.ICFile;
import com.idega.core.file.data.ICFileHome;
import com.idega.core.file.util.FileInfo;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.data.IDOLookup;
import com.idega.idegaweb.IWMainApplication;
import com.idega.portal.service.MediaResolver;
import com.idega.presentation.IWContext;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.StringUtil;
import com.idega.util.datastructures.map.MapUtil;

public class PortalMediaServlet extends HttpServlet implements Filter {

	private static final long serialVersionUID = -7982847048282024309L;

	private static final Logger LOGGER = Logger.getLogger(PortalMediaServlet.class.getName());

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		if (request instanceof HttpServletRequest) {
			HttpServletRequest req = (HttpServletRequest) request;
			if (req.getRequestURI().startsWith("/iw_cache") && response instanceof HttpServletResponse) {
				doGet(req, (HttpServletResponse) response);
				return;
			}
		}

		chain.doFilter(request, response);
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		String requestURI = req.getRequestURI();
		if (!StringUtil.isEmpty(requestURI) && requestURI.startsWith("/iw_cache")) {
			try {
				writeFileToResponse(requestURI, req, resp);
			} catch (Exception e) {
				LOGGER.log(Level.WARNING, "Error getting media: " + req.getRequestURI(), e);
			}

			return;
		}

		super.doGet(req, resp);
	}

	private FileInfo getFileInfo(IWMainApplication iwma, String fileUniqueId) {
		if (StringUtil.isEmpty(fileUniqueId)) {
			return null;
		}

		FileInfo fileInfo = null;
		try {
			IWContext iwc = CoreUtil.getIWContext();
			ServletContext context = iwc == null ? null : iwc.getServletContext();
			context = context == null ? iwma.getServletContext() : context;
			Map<String, MediaResolver> mediaResolvers = WebApplicationContextUtils.getWebApplicationContext(context).getBeansOfType(MediaResolver.class);
			if (!MapUtil.isEmpty(mediaResolvers)) {
				for (Iterator<MediaResolver> mediaResolversIter = mediaResolvers.values().iterator(); (fileInfo == null && mediaResolversIter.hasNext());) {
					fileInfo = mediaResolversIter.next().getFileInfo(fileUniqueId);
				}
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting file info: " + fileUniqueId, e);
		}

		return fileInfo;
	}

	private void writeFileToResponse(String requestURI, HttpServletRequest request, HttpServletResponse response) throws Exception {
		String fileUniqueId = MediaBusiness.getMediaId(requestURI);

		String name = null, type = null;
		Long size = Long.valueOf(0);
		InputStream source = null;

		try {
			IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
			String url = CoreConstants.EMPTY, fileToken = null;
			if (StringUtil.isEmpty(fileUniqueId)) {
				url = requestURI;
			} else {
				IWContext iwc = new IWContext(request, response, request.getServletContext());
				int underIndex = requestURI.indexOf(CoreConstants.UNDER);
				if (underIndex != -1) {
					String[] parts = requestURI.split(CoreConstants.UNDER);
					fileToken = ArrayUtil.isEmpty(parts) || parts.length < 3 ? null : parts[2];
				}
				url = ICFileSystemFactory.getFileSystem(iwma.getIWApplicationContext()).getFileURI(iwc, fileUniqueId, fileToken);
			}

			if (StringUtil.isEmpty(url)) {
				return;
			}

			FileInfo fileInfo = getFileInfo(iwma, fileUniqueId);
			if (fileInfo == null) {
				String realPath = iwma.getApplicationRealPath();

				if (realPath != null) {
					if (realPath.endsWith(CoreConstants.SLASH)) {
						realPath = realPath.substring(0, realPath.length() - 1);
					}
					File tmp = new File(realPath.concat(url));
					if (tmp.exists() && tmp.canRead()) {
						name = tmp.getName();
						size = tmp.length();
						source = new FileInputStream(tmp);
					}
				}
				if (source == null && !StringUtil.isEmpty(fileUniqueId)) {
					ICFileHome fileHome = (ICFileHome) IDOLookup.getHome(ICFile.class);
					ICFile file = fileHome.findByUUID(fileUniqueId);
					if (StringUtil.isEmpty(fileToken) || !fileToken.equals(file.getToken())) {
						return;
					}

					name = file.getName();
					Integer sizeTmp = file.getFileSize();
					size = sizeTmp == null ? null : sizeTmp.longValue();
					source = file.getFileValue();
					type = file.getMimeType();
				}

			} else {
				source = fileInfo.getSource();
				size = fileInfo.getContentLength();
				name = fileInfo.getFileName();
				type = fileInfo.getType();
			}

			if (source == null) {
				return;
			}

			if (size != null) {
				response.setContentLength(size.intValue());
			}
			if (StringUtil.isEmpty(type)) {
				type = MimeTypeUtil.resolveMimeTypeFromFileName(name);
			}
			if (!StringUtil.isEmpty(type)) {
				response.setContentType(type);
			}
			FileUtil.streamToOutputStream(source, response.getOutputStream());
		} finally {
			IOUtil.close(source);
		}
	}

}