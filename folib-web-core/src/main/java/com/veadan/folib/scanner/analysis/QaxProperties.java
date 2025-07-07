package com.veadan.folib.scanner.analysis;

import cn.hutool.core.util.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class QaxProperties {

    @Value("${thirdParty.qax.enable}")
    private Boolean enable;
    @Value("${thirdParty.qax.baseUrl}")
    private String baseUrl;
    @Value("${thirdParty.qax.privateToken}")
    private String privateToken;
    @Value("${thirdParty.qax.projectId}")
    private String projectId;
    @Value("${thirdParty.qax.username}")
    private String username;
    @Value("${thirdParty.qax.password}")
    private String password;

    @Value("${thirdParty.qax.tags}")
    private String tags;

    public String getBaseUrl() {
        return  StrUtil.trimToNull(baseUrl);
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = StrUtil.trimToNull(baseUrl);
    }

    public String getPrivateToken() {
        return  StrUtil.trimToNull(privateToken);
    }

    public void setPrivateToken(String privateToken) {
        this.privateToken = StrUtil.trimToNull(privateToken);
    }

    public String getProjectId() {
        return  StrUtil.trimToNull(projectId);
    }

    public void setProjectId(String projectId) {
        this.projectId = StrUtil.trimToNull(projectId);
    }

    public String getUsername() {
        return  StrUtil.trimToNull(username);
    }

    public String getPassword() {
        return  StrUtil.trimToNull(password);
    }

    public void setUsername(String username) {
        this.username = StrUtil.trimToNull(username);
    }

    public void setPassword(String password) {
        this.password = StrUtil.trimToNull(password);
    }

    public boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    public String getTags() {
        return  StrUtil.trimToNull(tags);
    }

    public void setTags(String tags) {
        this.tags = StrUtil.trimToNull(tags);
    }

}
