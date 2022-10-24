package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.veadan.folib.configuration.MutableVulnerabilityConfiguration;
import com.veadan.folib.configuration.VulnerabilityConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Optional;
import java.util.Set;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class VulnerabilitiesConfigurationForm
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

    public VulnerabilitiesConfigurationForm(Set<String> levels, Set<String> notifyScopes, Set<String> receiverUsers, Set<String> receiverEmails, Set<String> whites, Set<String> blacks) {
        this.levels = levels;
        this.notifyScopes = notifyScopes;
        this.receiverUsers = receiverUsers;
        this.receiverEmails = receiverEmails;
        this.whites = whites;
        this.blacks = blacks;
    }

    @JsonIgnore()
    public static VulnerabilitiesConfigurationForm fromConfiguration(VulnerabilityConfiguration source) {
        VulnerabilityConfiguration configuration = Optional.ofNullable(source).orElse(
                new VulnerabilityConfiguration(new MutableVulnerabilityConfiguration())
        );
        return new VulnerabilitiesConfigurationForm(configuration.getLevels(), configuration.getNotifyScopes(), configuration.getReceiverUsers(),
                configuration.getReceiverEmails(), configuration.getWhites(), configuration.getBlacks());
    }

    public interface WhiteGroup
            extends Serializable {
        // 白名单组
    }

    public interface BlackGroup
            extends Serializable {
        // 白名单组
    }

}
