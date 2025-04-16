package com.veadan.folib.users.userdetails;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.users.domain.Privileges;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2025/4/16
 **/
public class UserPrivileges {

    public static HttpServletRequest getRequest() {
        try {
            return ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        } catch (Exception ignore) {

        }
        return null;
    }

    public static boolean handlerRestrictedRepository(Collection<Privileges> grantedAuthorities, String storageId, String repositoryId) {
        HttpServletRequest request = getRequest();
        if (Objects.isNull(request)) {
            return false;
        }
        String serverName = request.getServerName();
        List<String> restrictedSourceList = getRestrictedSource();
        if (CollectionUtils.isEmpty(restrictedSourceList)) {
            return false;
        }
        if (!restrictedSourceList.contains(serverName)) {
            return false;
        }
        List<String> restrictedRepositoryList = getRestrictedRepository();
        if (CollectionUtils.isEmpty(restrictedRepositoryList)) {
            return false;
        }
        if (!restrictedRepositoryList.contains(ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId))) {
            grantedAuthorities.removeAll(Privileges.restricted());
            return true;
        }
        return false;
    }

    public static List<String> getRestrictedSource() {
        List<String> restrictedSourceList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_SOURCE";
        String restrictedSource = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedSource)) {
            restrictedSourceList = Arrays.asList(restrictedSource.split(","));
        }
        return restrictedSourceList;
    }

    public static List<String> getRestrictedRepository() {
        List<String> restrictedRepositoryList = Lists.newArrayList();
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String key = "RESTRICTED_REPOSITORY";
        String restrictedRepository = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(restrictedRepository)) {
            restrictedRepositoryList = Arrays.asList(restrictedRepository.split(","));
        }
        return restrictedRepositoryList;
    }
}
