package com.veadan.folib.interceptors;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.config.PermissionCheck;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.scanner.common.util.IPUtil;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.wrapper.RequestWrapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.*;

/**
 * 自定义权限拦截器 校验节点间请求的白名单
 *
 * @author qijianping
 */
@Slf4j
public class PermissionCheckInterceptor implements HandlerInterceptor {
    private final Set<String> currentWhiteList = new HashSet<>();

    public Set<String> getWhiteList(String ipAddr) {
        if (currentWhiteList.contains(ipAddr)) {
            return currentWhiteList;
        }
        ConfigurationManagementService configurationManagementService =
                SpringUtil.getBean(ConfigurationManagementService.class);
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        if (CollectionUtil.isEmpty(map)) {
            return currentWhiteList;
        }
        map.values().forEach(clusterDispatchNodeDto -> {
            try {
                String host = clusterDispatchNodeDto.getClusterNodeHost();
                host = host.replaceAll("http|https|//|/", "");
                String[] arry = host.split(":");
                if (arry.length >= 2) {
                    currentWhiteList.add(arry[arry.length - 2].trim());
                }
            } catch (Exception e) {
                log.error("Exception {}", e.getMessage());
            }
        });
        return currentWhiteList;
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
                             Object handler) throws Exception {
        HandlerMethod handlerMethod = (HandlerMethod) handler;
        PermissionCheck permission = findPermissionCheck(handlerMethod);

        //如果没有添加权限注解则直接跳过允许访问
        if (permission == null) {
            return true;
        }
        //获取注解中的值
        String resourceKey = permission.resourceKey();

        //是否在白名单中
        String ipAddr = IPUtil.getIpAddr(request);
        log.info("当前调用ip {} ", ipAddr);
        if (getWhiteList(ipAddr).contains(ipAddr)) {
            log.info("{} 白名单调用 {}", ipAddr, handlerMethod.toString());
            return true;
        }
        //获取用的角色权限列表中是否拥有该权限
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (!authentication.isAuthenticated()) {
            handlerResponse(response);
            return false;
        }
        boolean auth = authentication.getAuthorities().stream().anyMatch(item -> item.getAuthority().equals(resourceKey));
        if (auth) {
            return true;
        }
        String storageKey = permission.storageKey();
        String repositoryKey = permission.repositoryKey();
        if (StringUtils.isNotBlank(storageKey) && StringUtils.isNotBlank(repositoryKey)) {
            String storageId = request.getParameter(storageKey);
            String repositoryId = request.getParameter(repositoryKey);
            String filePathMap = request.getParameter("filePathMap");
            List<String> filePaths = null;
            if (StringUtils.isNotBlank(filePathMap)) {
                JSONObject filePathJson = JSONObject.parseObject(request.getParameter("filePathMap"));
                Iterator<String> iterable = filePathJson.keySet().iterator();
                String filePathKey = "", filePath;
                filePaths = Lists.newArrayList();
                while (iterable.hasNext()) {
                    filePathKey = iterable.next();
                    filePath = filePathJson.getString(filePathKey);
                    if (StringUtils.isNotBlank(filePath)) {
                        filePaths.add(filePath);
                    }
                }
            }
            String body = StringUtils.EMPTY;
            if (request instanceof RequestWrapper) {
                body = ((RequestWrapper) request).getBody();
            }
            if (StringUtils.isNotBlank(body)) {
                JSONObject jsonObject = JSONObject.parseObject(body);
                storageId = jsonObject.getString(storageKey);
                repositoryId = jsonObject.getString(repositoryKey);
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(storageId, repositoryId, filePaths);
            boolean flag = storageAuthorities.stream().anyMatch(item -> item.getAuthority().equals(resourceKey));
            if (!flag) {
                handlerResponse(response);
            }
            return flag;
        }
        handlerResponse(response);
        return false;
    }

    /**
     * 根据handlerMethod返回注解信息
     *
     * @param handlerMethod 方法对象
     * @return PermissionCheck注解
     */
    private PermissionCheck findPermissionCheck(HandlerMethod handlerMethod) {
        //在方法上寻找注解
        PermissionCheck permission = handlerMethod.getMethodAnnotation(PermissionCheck.class);
        if (permission == null) {
            //在类上寻找注解
            permission = handlerMethod.getBeanType().getAnnotation(PermissionCheck.class);
        }
        return permission;
    }

    private void handlerResponse(HttpServletResponse response) {
        try {
            response.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            String msg = "Access to the requested resource is not authorized.";
            response.getWriter().println(JSONObject.toJSONString(new ErrorResponseEntityBody(msg)));
            response.flushBuffer();
        } catch (Exception ex) {
            log.error("处理响应失败：{}", ExceptionUtils.getStackTrace(ex));
        }
    }
}
