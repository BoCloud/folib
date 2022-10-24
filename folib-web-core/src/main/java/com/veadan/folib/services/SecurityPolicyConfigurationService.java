package com.veadan.folib.services;

import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;

import java.io.IOException;

/**
 * @author leipenghui
 * @date 2022/10/21
 **/
public interface SecurityPolicyConfigurationService {

    /**
     * 设置平台级别白名单
     *
     * @param whites 平台级别白名单
     * @throws IOException io异常
     */
    void setVulnerabilitiesWhites(String whites) throws IOException;

    /**
     * 设置平台级别黑名单
     *
     * @param blacks 平台级别黑名单
     * @throws IOException io异常
     */
    void setVulnerabilitiesBlacks(String blacks) throws IOException;

    /**
     * 添加平台级别白名单
     *
     * @param white 平台级别白名单
     * @throws IOException io异常
     */
    void addVulnerabilitiesWhite(String white) throws IOException;

    /**
     * 添加平台级别黑名单
     *
     * @param black 平台级别黑名单
     * @throws IOException io异常
     */
    void addVulnerabilitiesBlack(String black) throws IOException;

    /**
     * 删除平台级别白名单
     *
     * @param white 平台级别白名单
     * @throws IOException io异常
     */
    void removeVulnerabilitiesWhite(String white) throws IOException;

    /**
     * 删除平台级别黑名单
     *
     * @param black 平台级别黑名单
     * @throws IOException io异常
     */
    void removeVulnerabilitiesBlack(String black) throws IOException;

    /**
     * 保存或者更新平台通知配置
     *
     * @param securityPolicyConfigurationForm 参数
     * @throws IOException io异常
     */
    void saveOrUpdateNotify(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException;

    /**
     * 保存或者更新平台阻断配置
     *
     * @param securityPolicyConfigurationForm 参数
     * @throws IOException io异常
     */
    void saveOrUpdateBlock(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException;

    /**
     * 查询安全策略配置
     *
     * @return 安全策略配置
     */
    SecurityPolicyConfigurationForm config();
}
