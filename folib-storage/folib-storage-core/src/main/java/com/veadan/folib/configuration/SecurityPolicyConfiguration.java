package com.veadan.folib.configuration;


import com.google.common.collect.Sets;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * @author leipenghui
 */
@Immutable
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SecurityPolicyConfiguration
        implements Serializable {

    /**
     * 漏洞等级
     */
    private Set<String> levels;
    /**
     * 白名单列表
     */
    private Set<String> whites;
    /**
     * 黑名单列表
     */
    private Set<String> blacks;
    /**
     * 通知范围
     */
    private Set<String> notifyScopes;
    /**
     * 指定用户
     */
    private Set<String> receiverUsers;
    /**
     * 指定邮箱
     */
    private Set<String> receiverEmails;
    /**
     * 阻断类型 1 全量阻断 2 黑名单阻断 3 包名阻断
     */
    private Integer blockType;
    /**
     * 阻断漏洞等级
     */
    private Set<String> blockLevels;
    /**
     * 过滤白名单
     */
    private Boolean filterWhites;
    /**
     * 包名
     */
    private Set<String> packageNames;

    public SecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        this.levels = mutableSecurityPolicyConfiguration.getLevels();
        this.whites = mutableSecurityPolicyConfiguration.getWhites();
        this.blacks = mutableSecurityPolicyConfiguration.getBlacks();
        this.notifyScopes = mutableSecurityPolicyConfiguration.getNotifyScopes();
        this.receiverUsers = mutableSecurityPolicyConfiguration.getReceiverUsers();
        this.receiverEmails = mutableSecurityPolicyConfiguration.getReceiverEmails();
        this.blockType = mutableSecurityPolicyConfiguration.getBlockType();
        this.blockLevels = mutableSecurityPolicyConfiguration.getBlockLevels();
        this.filterWhites = mutableSecurityPolicyConfiguration.getFilterWhites();
        this.packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
    }

    public Set<String> getLevels() {
        return Objects.isNull(levels) ? Sets.newLinkedHashSet() : levels;
    }

    public Set<String> getWhites() {
        return Objects.isNull(whites) ? Sets.newLinkedHashSet() : whites;
    }

    public Set<String> getBlacks() {
        return Objects.isNull(blacks) ? Sets.newLinkedHashSet() : blacks;
    }

    public Set<String> getBlockLevels() {
        return Objects.isNull(blockLevels) ? Sets.newLinkedHashSet() : blockLevels;
    }

}
