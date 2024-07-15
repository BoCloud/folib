

package com.veadan.folib.scanner.common.base;


import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.users.security.SecurityTokenProvider;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.*;

/**
 * 基础业务类
 *
 * @author Veadan
 * @version 2018/1/13.
 */
public abstract class BusinessBiz<M extends CommonMapper<T>, T> extends BaseBiz<M, T> implements JwtTokenFetcher {

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Override
    public void insertSelective(T entity) {
        super.insertSelective(entity);
    }

    @Override
    public void updateById(T entity) {
        super.updateById(entity);
    }

    @Override
    public void updateSelectiveById(T entity) {
        super.updateSelectiveById(entity);
    }

    /**
     * 获取登录用户名
     *
     * @return 登录用户名
     */
    protected String loginUsername() {
        HttpServletRequest request = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getRequest();
        Optional<String> token = getToken(request);
        String username = securityTokenProvider.getSubject(token.get());
        return username;
    }

    /**
     * 获取配置信息
     *
     * @return 配置信息
     */
    protected MutableConfiguration mutableConfiguration() {
        return configurationManagementService.getMutableConfigurationClone();
    }

    /**
     * 获取没有权限访问的存储空间id列表
     *
     * @return 没有权限访问的存储空间id列表
     */
    public List<String> withoutPermissionStorageIdList() {
        List<String> allStorageIdList = new ArrayList<String>(mutableConfiguration().getStorages().keySet());
        List<String> storageIdList = havePermissionStorageIdList();
        allStorageIdList.removeAll(storageIdList);
        return allStorageIdList;
    }

    /**
     * 获取有权限访问的存储空间id列表
     *
     * @return 有权限访问的存储空间id列表
     */
    public List<String> havePermissionStorageIdList() {
        List<String> storageIdList = Lists.newArrayList();
        String username = loginUsername();
        String admin = "admin";
        if (admin.equals(username)) {
            storageIdList = new ArrayList<>(mutableConfiguration().getStorages().keySet());
            return storageIdList;
        }
        for (Map.Entry<String, StorageDto> entry : mutableConfiguration().getStorages().entrySet()) {
            Set<String> userSet = entry.getValue().getUsers();
            if (CollectionUtils.isNotEmpty(userSet)) {
                if (userSet.contains(username)) {
                    storageIdList.add(entry.getKey());
                }
            }
        }
        return storageIdList;
    }

    /**
     * 获取基础查询参数
     *
     * @return 基础查询参数
     */
    public BaseQuery getBaseQuery() {
        List<String> storageIdList = havePermissionStorageIdList();
        List<String> notInStorageIdList = withoutPermissionStorageIdList();
        return BaseQuery.builder().notInStorageIdList(notInStorageIdList).storageIdList(storageIdList).build();
    }
}
