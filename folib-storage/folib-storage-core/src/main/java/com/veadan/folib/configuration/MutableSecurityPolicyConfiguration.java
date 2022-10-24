package com.veadan.folib.configuration;

import com.beust.jcommander.internal.Sets;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * @author leipenghui
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MutableSecurityPolicyConfiguration
        implements Serializable {

    /**
     * 漏洞等级
     */
    private Set<String> levels = Sets.newLinkedHashSet();
    /**
     * 白名单列表
     */
    private Set<String> whites = Sets.newLinkedHashSet();
    /**
     * 黑名单列表
     */
    private Set<String> blacks = Sets.newLinkedHashSet();
    /**
     * 通知范围
     */
    private Set<String> notifyScopes = Sets.newLinkedHashSet();
    /**
     * 指定用户
     */
    private Set<String> receiverUsers = Sets.newLinkedHashSet();
    /**
     * 指定邮箱
     */
    private Set<String> receiverEmails = Sets.newLinkedHashSet();
    /**
     * 阻断类型 1 全量阻断 2 黑名单阻断
     */
    private Integer blockType;
    /**
     * 阻断漏洞等级
     */
    private Set<String> blockLevels = Sets.newLinkedHashSet();
    /**
     * 过滤白名单
     */
    private Boolean filterWhites = false;

    public void addWhite(String white) {
        this.whites.add(white);
    }

    public void addBlack(String black) {
        this.blacks.add(black);
    }

}
