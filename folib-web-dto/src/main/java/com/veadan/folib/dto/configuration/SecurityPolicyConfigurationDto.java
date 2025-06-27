package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.configuration.SecurityPolicyConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.Optional;
import java.util.Set;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SecurityPolicyConfigurationDto
        implements Serializable {

    /**
     * 白名单列表
     */
    @NotBlank(message = "请填写白名单", groups = {WhiteGroup.class})
    private String white;

    /**
     * 黑名单列表
     */
    @NotBlank(message = "请填写黑名单", groups = {BlackGroup.class})
    private String black;

    /**
     * 漏洞等级
     */
    private Set<String> levels;
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
     * 白名单列表
     */
    private Set<String> whites;
    /**
     * 黑名单列表
     */
    private Set<String> blacks;
    /**
     * 阻断类型 1 全量阻断 2 黑名单阻断 3 包名阻断
     */
    @NotNull(message = "请选择阻断方式", groups = {BlockGroup.class})
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

    /**
     * 过期时间
     */
    private Date sellByDate;

    public SecurityPolicyConfigurationDto(SecurityPolicyConfiguration securityPolicyConfiguration) {
        this.levels = securityPolicyConfiguration.getLevels();
        this.notifyScopes = securityPolicyConfiguration.getNotifyScopes();
        this.receiverUsers = securityPolicyConfiguration.getReceiverUsers();
        this.receiverEmails = securityPolicyConfiguration.getReceiverEmails();
        this.whites = securityPolicyConfiguration.getWhites();
        this.blacks = securityPolicyConfiguration.getBlacks();
        this.blockType = securityPolicyConfiguration.getBlockType();
        this.blockLevels = securityPolicyConfiguration.getBlockLevels();
        this.filterWhites = securityPolicyConfiguration.getFilterWhites();
        this.packageNames = securityPolicyConfiguration.getPackageNames();
    }

    @JsonIgnore()
    public static SecurityPolicyConfigurationDto fromConfiguration(SecurityPolicyConfiguration source) {
        SecurityPolicyConfiguration configuration = Optional.ofNullable(source).orElse(
                new SecurityPolicyConfiguration(new MutableSecurityPolicyConfiguration())
        );
        return new SecurityPolicyConfigurationDto(configuration);
    }

    public interface WhiteGroup
            extends Serializable {
        // 白名单组
    }

    public interface BlackGroup
            extends Serializable {
        // 白名单组
    }

    public interface BlockGroup
            extends Serializable {
        // 阻断组
    }

}
